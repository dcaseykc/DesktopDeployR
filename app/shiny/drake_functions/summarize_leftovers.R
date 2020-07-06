#left = readd(leftovers, cache=  cache)
summarize_leftovers = function(left){
  
  left = left[, .(`Remaining Items` = sum(each_per_su * leftover)), .(item_type, size)]
  
  return(left)
  
}