# ppe = readd(ppe, cache = cache)
# inv = readd(inv, cache = cache)
# ltcf = readd(ltcf, cache = cache)
# hospital = readd(hospital, cache = cache)
# ignore_items = ""
# thetiers = runtiers
# inv_mismatch = F
order_filler = function(ppe, inv, ltcf, hospital, thetiers, ignore_items = "", inv_mismatch = FALSE, n95except = ""){
  
  ppe = copy(ppe)
  ppe[agency %in% ltcf[is.na(cases), agency], tier := 1.5]
  ppe[, fill_me := 1]
  
  #only items we have
  req_no_inv = sort(setdiff(ppe$itemz, inv$itemz))
  inv_no_req = sort(setdiff(inv$itemz, ppe$itemz))
  
  if(inv_mismatch){
    
    if(length(req_no_inv) > length(inv_no_req)){
      inv_no_req = c(inv_no_req, rep(NA, length(req_no_inv) - length(inv_no_req)))
    }else{
      req_no_inv = c(req_no_inv, rep(NA, length(inv_no_req) - length(req_no_inv)))
    }
    
    return(data.table(req_no_inv = req_no_inv, inv_no_req = inv_no_req))
  }
  
  warning(paste('The following item types were requested, but are not in inventory:', paste(req_no_inv, collapse = ' | ')))
  warning(paste('The following item types are in inventory, but not requested:', paste(inv_no_req, collapse = ' | ')))
  
  #items we don't have in stock, but maybe elgible for replacement
  ppe[itemz %in% req_no_inv, fill_me := -1]
  
  #only the tier we are running for
  ppe[!tier %in% thetiers, fill_me := 0]
  
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
  
  ppe[!(tier %in% c(0,1,1.25) | type %in% n95except) & item_type == 'N95', fill_me := 0]
  
  stopifnot(nrow(ppe[itemz == 'mask, any size', .N, order_ids][N>1]) == 0)
  

  
  
  return(ppe)
  
}
