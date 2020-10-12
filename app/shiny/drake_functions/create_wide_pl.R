# loadd(allocations, ads, cache = cache)
create_wide_pl <- function(allocations, ads, standardize_chinook = TRUE){
  dcols = c('lnum', 'ppe_id', 'fill_me', 'droppies', 'percent_filled', 'assigned', 'ord_id', 'allocated', 'item_type', 'size', 'itemz', 'requested')
  allocs = lapply(allocations, function(x) x[allocated>0, .SD, .SDcols = setdiff(names(x), dcols)])
  
  allocs = rbindlist(allocs, fill = T)
  
  overs = c('agency', 'type', 'order_ids', 'tier', 'priority')
  allocs = allocs[, lapply(.SD, function(x) sum(x, na.rm = T)),
                  by = overs,
                  .SDcols = setdiff(names(allocs), overs)]
  
  stopifnot(all(allocs[,.N, order_ids][,N==1]))
  
  for(nnn in setdiff(names(allocs), overs)){
    
    if(all(allocs[, (get(nnn)) == 0])){
      allocs[, (nnn) := NULL]
    }
    
  }
  
  
  pl = add_delivery_info(allocs, ads)
  
  if(standardize_chinook){
    pl[grepl('401 5th', Address, fixed = TRUE) | grepl('chinook', tolower(Address), fixed = T), Address := '401 5th Ave Seattle WA 98104; Room 121/123']
  }
  
  return(pl)
  
}
