# loadd(pl_wide, ads, cache = cache)
# pl = copy(pl_wide)
# template
# outpath = oot_excel
# tiers =unique(pl$tier)
save_picklist= function(pl, template, outpath, tiers =unique(pl$tier)){
  
  pl = pl[tier %in% tiers, ]
  
  wb <- loadWorkbook(template)
  
  #for each row in the pick list, create a tab in the workbook
  if(nrow(pl)>0){
    for(rrr in 1:nrow(pl)){
      
      this_pl = pl[rrr,]
      
      #create the PL page
      wb_name = substr(paste0(rrr, ' ', this_pl$agency), 1, 30)
      wb_name = gsub("[^[:alnum:] ]", "", wb_name)
      #print(wb_name)
      cloneWorksheet(wb, wb_name, names(wb)[1])
      
      #fill in the things
      writeData(wb, wb_name, paste0('Tier: ', this_pl[, tier] ), xy = c(1,1))
      writeData(wb, wb_name, paste0('Order Time: ', Sys.time() ), xy = c(1,2))
      writeData(wb, wb_name, paste0('Issued to: ', this_pl[, agency]), xy = c(1,3))
      writeData(wb, wb_name, paste0('Loc: ', this_pl[, Address]), xy = c(1,4))
      writeData(wb, wb_name, paste0('Name: ', this_pl[, POC]), xy = c(1,5))
      writeData(wb, wb_name, paste0('Orders: ', this_pl[, order_ids]), xy = c(1,8))
      writeData(wb, wb_name, paste0('Phone: ', this_pl[, Phone]), xy = c(1,6))
      writeData(wb, wb_name, paste0('Email: ', this_pl[, email]), xy = c(1,7))
      #write the matrix of items
      agency = this_pl$agency
      this_pl[, id := ""]
      this_pl = melt(this_pl[, .SD, .SDcols = setdiff(names(this_pl), c('order_ids', 'agency', 'Address', 'Phone', 'POC', 'email', 'type', 'tier', 'priority'))], 
                     id.vars = 'id', variable.factor = F, value.factor = F)
      this_pl = this_pl[value > 0]
      this_pl[, id:= substr(variable, 1, regexpr(':', variable, fixed = T)-1)]
      this_pl[, Description := trimws(substr(variable, regexpr(':', variable, fixed = T)+1, nchar(variable)))]
      this_pl = this_pl[!is.na(value)]
      
      this_pl = this_pl[, .(Line = .I, Item =id, Description, Quantity = value, `Qty Picked` = "")]
      
      writeDataTable(wb, wb_name, this_pl, startCol = 1, startRow = 15, colNames = TRUE, tableStyle = 'TableStyleMedium18', withFilter = F,
                     bandedRows = TRUE)
      
      
      #lines
      sss = createStyle(border = 'TopBottom')
      # rrr = 13:nrow(this_pl)
      # cols = 1:5
      addStyle(wb, wb_name,rows = 15:(15+nrow(this_pl)), stack = TRUE,gridExpand = T, style = sss, cols = 1:5)
      
      
    }
    removeWorksheet(wb, names(wb)[1])
    saveWorkbook(wb, file = outpath, overwrite = TRUE )
    
    return(T)
  }else{
    return(F)
  }
}
