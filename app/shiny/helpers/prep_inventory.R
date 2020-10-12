#' Convert the inventory report from maximo/warehouse to a format usable for analysis
#' @param fold directory path- Location of the working directory
#' @param date date- Date of the cycle
#' @param old file path- Location of the previous inventory. Classifications will be carried forward
#' @param new file path- Location of the current inventory (without the metadata)
#' @param version character/numeric - Version identifier for the resulting files. Choosing an existing inventory version will cause an overwrite
#' @return file path to the saved inventory file
prep_inventory = function(fold, date, old, new, version){
  cycle_mo = month(date)
  cycle_day = mday(date)
  
  old = load_spreadsheet(old)
  
  #The inventory generally comes in two ways-- the xml pretending to be xls from the raw extract or as an actual xls file
  #because there are two different main approachs, the following code tries to figure out how to read/format them the same
  #try reading as an xml
  hold = try(read_xml(new), silent = TRUE)
  if(!inherits(hold, 'try-error')){
    new = hold
    new = as_list(new)
    
    #extract the table
    new = new[['Workbook']][['Worksheet']][['Table']]
    
    #find the row where the data starts
    extract_rowdata = function(x){
      dat = unlist(lapply(x, function(y) y$Data[[1]]))
      return(dat)
    }
    
    dat = lapply(new, extract_rowdata)
    
    #find the header row
    header =which(vapply(dat, function(x){
      if(length(x)>0){
        return(x[1] %in% 'Item')
      }else{
        FALSE
      }}, T))
    
    stopifnot(length(header) == 1)
    
    datarows =which(vapply(dat, function(x){
      if(length(x)>0){
        if(x[1] %in% '1424') return(TRUE)
        
        return(substr(x[1],1,3) == 'C19')
        
      }else{
        FALSE
      }}, T))
    
    #construct dataset
    dt = lapply(datarows, function(x) as.data.table(t(dat[[x]])))
    dt = rbindlist(dt, fill = T )
    setnames(dt, dat[[header]][seq_len(ncol(dt))])
    
  }else{
    dt = setDT(read_excel(new))
    setnames(dt, unlist(dt[4,]))
    dt = dt[5:nrow(dt)]
    dt = dt[1:(min(which(is.na(dt[, Item])))-1)]
  }
  new = copy(dt)
  
  new = merge(new, old[, .(each_per_su, item_type, size, Item, Description)], all.x = T, by = c('Item', 'Description'))
  new[, ship_u := `Qty Avbl`]
  
  #set items Not For General Distribution to 0
  new[grepl('NFGD', Description), ship_u := -1]
  
  write.csv(new, file.path(fold, 'inv', paste0('inventory_',cycle_mo, cycle_day, '_', version,'.csv')), row.names = F, na = "")
  return(file.path(fold, 'inv', paste0('inventory_',cycle_mo, cycle_day, '_', version,'.csv')))
}