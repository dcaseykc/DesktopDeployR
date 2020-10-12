#' Prepare the hospital data for analysis
#' @param fold directory path- location of the working directory
#' @param date cycle date
#' @param ordersandtiers_version numeric/character- version of the orders and tiers files to use
qa_orders = function(fold, 
                    date,
                    ordersandtiers_version){
  
  #Load cycle information
  cycle_day = mday(date)
  cycle_mo = month(date)
  
  ot_v = ordersandtiers_version
  suffix = paste0('_', cycle_mo,cycle_day)
  output = file.path(fold)
  out_qa_order = file.path(output, paste0('qa_orders', suffix,'.csv'))
  
  items = setDT(load_spreadsheet(file.path(fold, paste0('item_classifications.csv'))))
  items[, item := trimws(trimws(item, whitespace = "[\\h\\v]"))]
  
  orders = setDT(load_spreadsheet(file.path(fold, paste0('order_list_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))))
  orders = orders[, c('wa_num','item_requested','requested','email','POC')]
  orders[, item_requested := trimws(trimws(item_requested, whitespace = "[\\h\\v]"))]

  tiering = setDT(load_spreadsheet(file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))))
  tiering = select (tiering,-c(logs_lnum,logs_type,logs_tier))
  
  # join item classification to orders
  order_items = merge(orders, items, all.x = T, by.x = 'item_requested', by.y = 'item')
  
  # join orders to tiers
  qa_file = merge(order_items, tiering, all.x = T, by = 'wa_num')
  
  # wa_num	item_requested	requested	email	POC	item_type	size	agency	address	zip	region	lnum	type	newname	notes	current.tier	priority
  qa_file = select (qa_file, c(wa_num,agency,type,item_type,requested,current.tier,priority,region,zip,address,email,POC,lnum,newname))
  
  #print full
  write.csv(qa_file,file = out_qa_order, row.names = F, na = '')
  print(paste0('QA file created ',out_qa_order))
} 
