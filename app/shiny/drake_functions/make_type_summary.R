# asum = readd(sum_full, cache = cache)
# type = 'hospital'
make_type_summary = function(asum, type,  oot1, oot2){
  thetype = type
  asum = asum[type == thetype]
  
  #collapse by item type
  itsum = asum[, .(requested = sum(requested), allocated = sum(allocated), type = paste0('All ', thetype)), by = item_type]
  itsum[, `percent filled` := round(allocated/requested * 100)]
  
  write.csv(asum[, .(order_ids, agency, type, item_type, size, tier, fill_me, requested, allocated)], file = oot1, row.names = F)
  write.csv(itsum, file = oot2, row.names = F)
  
  return(list(asum, itsum))
}