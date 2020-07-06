# asum = readd(sum_full, cache = cache)
summarize_cycle = function(asum){
  
  a = asum[fill_me != 0, .(requested = sum(requested), allocated = sum(allocated)), by = 'item_type']
  a[, percent := round(allocated/requested * 100)]
  
  # b = asum[fill_me == 1, .(requested = sum(requested), allocated = sum(allocated)), by = 'item_type']
  # b[, percent := round(allocated/requested * 100)]
  
  return(a)
  
}