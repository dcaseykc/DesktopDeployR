distribution_report = function(allocations, inv){

  allocs = rbindlist(lapply(allocations, function(x) x[, .SD, .SDcols = c('order_ids', 'agency', intersect(names(x), inv[,Item_long]))]),fill = T)  
  
  allocs = melt(allocs, id.vars = c('order_ids', 'agency'), variable.factor = F, variable.name = 'Item_long', value.name = 'allocated')
  
  allocs = merge(allocs, inv, all.x = T, by = "Item_long")
  
  allocs[, each := allocated * each_per_su]
  allocs = allocs[!is.na(each) & each > 0]
  
  allocs = allocs[, .(order_ids, agency, item_id = id, description = Item, each)]
  
  return(allocs)
    
}

