#' Create a sheet for classifications of items.
#' @param fold file path of the output/working folder
#' @param date date of the cycle
#' @param old file path to the old classification list
#' @param new file path to the new orders
#' @return file path to the resulting item classifications file
prep_item_classifications = function(fold, date, old, new){
  cycle_mo = month(date)
  cycle_day = mday(date)
  order_v = new
  
  new = load_spreadsheet(file.path(fold, paste0('order_list_', cycle_mo, cycle_day,'_', order_v, '.xlsx')))
  old = load_spreadsheet(old)
  
  old[, item := trimws(trimws(item, whitespace = "[\\h\\v]"))]
  new[, item := trimws(trimws(item_requested, whitespace = "[\\h\\v]"))]
  
  new = unique(new[, .(item)])
  new = rbind(old, new[!item %in% old[, item]], fill = T)
  
 
  
  out = file.path(fold, 'item_classifications.csv')
  
  write.csv(unique(new), row.names = F, file = out, na = "")
  
  return(out)
  
}
