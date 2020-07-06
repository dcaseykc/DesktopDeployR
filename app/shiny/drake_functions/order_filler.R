# ppe = readd(ppe, cache = cache)
# inv = readd(inv, cache = cache)
# ltcf = readd(ltcf, cache = cache)
# hospital = readd(hospital, cache = cache)
# ignore_items = ""
# thetiers = runtiers
# inv_mismatch = F
order_filler = function(ppe, inv, ltcf, hospital, thetiers, ignore_items = "", inv_mismatch = FALSE){
  
  ppe = copy(ppe)
  ppe[agency %in% ltcf[is.na(cases), agency], tier := 1.5]
  ppe[, fill_me := 1]
  
  #only the tier we are running for
  ppe[!tier %in% thetiers, fill_me := 0]
  
  #only items we have
  req_no_inv = setdiff(ppe$itemz, inv$itemz)
  inv_no_req = setdiff(inv$itemz, ppe$itemz)
  
  if(inv_mismatch){
    return(list(req_no_inv = req_no_inv, inv_no_req = inv_no_req))
  }
  
  warning(paste('The following item types were requested, but are not in inventory:', paste(req_no_inv, collapse = ' | ')))
  warning(paste('The following item types are in inventory, but not requested:', paste(inv_no_req, collapse = ' | ')))
  
  #manual adjustments
  ppe[item_type %in% 'testing, test kits', fill_me := 0]
  
  #drop hospitals with too much stuff
  mis_hosp = setdiff(ppe[type == 'hospital', agency], hospital$hosp_items[,name])
  
  if(length(mis_hosp)>0){
    stop(paste0('The following hospitals have requests, but no corrosponding inventory information:', paste0(mis_hosp, collapse = ', ')))
  }
  
  ppe = merge(ppe, hospital$hosp_items[, .(agency = name, item_type, droppies)], all.x = T, by = c('item_type', 'agency'))
  ppe[droppies %in% 1, fill_me := 0]
  
  ## don't assign certain items that require more/manual thought
  ppe[item_type %in% ignore_items, fill_me := 0]
  
  ## home health is handled by DSHS
  #ppe[grepl('home health', type), fill_me := 0]
  
  ## other odds and ends
  ## todo: turn this into a warning
  ppe[requested <= 0 | is.na(requested), fill_me := 0]
  ppe[type == 'ems, not bulk', fill_me := 0]
  
  ## make sure all tiered requests have a facility type
  stopifnot(ppe[fill_me==1, sum(is.na(type))]==0)
  
  #N95s in ltcfs
  #which ltcfs inappropriately requested N95s
  bad_n95 = ppe[!(tier %in% c(0,1, 1.25) | type %in% c('meo', 'health clinic, community', 'congregate, jail')) & item_type == 'N95', ]
  
  #for those that didn't ask for masks, give them a mask request of the same size
  mask_replace = (bad_n95[!agency %in% ppe[item_type == 'mask', agency], ])
  mask_replace[, c('fill_me', 'ppe_id', 'item_type', 'size', 'itemz') := .(fill_me, -ppe_id, 'mask', 'any size', 'mask, any size')]
  mask_replace = mask_replace[, .(requested = sum(requested), ppe_id = paste0(ppe_id, collapse = ', ')), by = setdiff(names(mask_replace), c('requested', 'ppe_id'))]
  
  #for those with masks already
  mask_add = bad_n95[agency %in% ppe[item_type == 'mask', agency], .(add_me = sum(requested)), by = .(order_ids)][, itemz := paste0('mask',', ', 'any size')]
  
  ppe = merge(ppe, mask_add, all.x = T, by = c('order_ids', 'itemz'))
  ppe[!is.na(add_me), requested := requested + add_me]
  ppe[,add_me := NULL]
  ppe = rbind(ppe, mask_replace)
  
  
  ppe[!(tier %in% c(0,1,1.25) | type %in% c('meo', 'health clinic, community', 'congregate, jail', 'law enforcement')) & item_type == 'N95', fill_me := 0]
  
  stopifnot(nrow(ppe[itemz == 'mask, any size', .N, order_ids][N>1]) == 0)
  
  ppe[itemz %in% req_no_inv, fill_me := -1]
  
  #MAT has very specific N95 requests
  ppe[agency %in% 'PHSKC - MAT' & item_type %in% 'N95' & size %in% c('s','r'), fill_me := 0]
  
  
  return(ppe)
  
}
