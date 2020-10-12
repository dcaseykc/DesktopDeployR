# loadd(inv, allocations, cache = cache)
find_leftovers <- function(inv, allocations){
  
  allocs = rbindlist(lapply(allocations, function(x) x[, .SD, .SDcols = intersect(names(x), inv[,Item_long])]),fill = T)  
  allocs = allocs[, lapply(.SD, function(x) sum(x, na.rm = T))]
  allocs[, blah := 1]
  
  allocs = melt(allocs, id.vars = 'blah', variable.factor = F, variable.name = 'Item_long', value.name = 'minus')
  
  inv = merge(inv, allocs, all.x = T, by = "Item_long")
  
  stopifnot(all(inv[!is.na(minus), minus <= ship_u]))
  
  inv[is.na(minus), minus := 0]
  inv[, leftover := ship_u - minus]
  
  inv = inv[, .(Source, Item, item_type, size, each_per_su, ship_u, minus, leftover)]
  
  return(inv)
  
}