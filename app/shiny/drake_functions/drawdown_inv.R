drawdown_inv = function(alloc, inv){
  inv = copy(inv)
  
  if(is.data.table(alloc)){
    drawdown = copy(alloc)
  }else{
    drawdown = rbindlist(alloc, fill = T)
  }
  
  drawdown = drawdown[, .SD, .SDcols = intersect(names(drawdown), inv[, Item_long])]
  if(nrow(drawdown) >0){
    drawdown = drawdown[, lapply(.SD, function(x) sum(x, na.rm = T)), .SDcols = names(drawdown)]
    drawdown[, id := 1]
    drawdown = melt(drawdown, id.vars = 'id', variable.factor = FALSE, variable.name = 'Item_long', value.name = 'minus')
    drawdown[, id := NULL]
    inv = merge(inv, drawdown, all.x = T)
    inv = inv[!is.na(minus), ship_u := ship_u-minus]
    inv[, minus := NULL]
  }
  return(inv)
}
