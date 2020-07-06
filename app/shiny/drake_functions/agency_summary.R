# loadd(orders, allocations, cache = cache)
# type = 'all'
# tiers = runtiers
agency_summary = function(allocations, type = c('all'), outpath, tiers = 1:3){
  fuf = lapply(allocations, function(x) x[, .(order_ids, lnum, agency, type, item_type, size, tier, priority, fill_me, requested, assigned, allocated)])
  fuf = rbindlist(fuf)
  
  fuf[, percent_filled := round(allocated/requested*100)]
  
  stopifnot(length(type) == 1)

  if(type %in% 'considered'){

    fuf = fuf[tier %in% tiers,]
  }

  if(type %in% 'fillable'){
    fuf = fuf[fill_me != 0, ]
  }
  
  write.csv(fuf, row.names = F, file = outpath, na = '')
  
  return(fuf)
  
}
