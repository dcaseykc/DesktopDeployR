# loadd(ads, allocations, cache = cache)
# pl_wide = create_wide_pl(allocations)
# pl = copy(pl_wide)
add_delivery_info = function(pl, ads){
  
  for(i in 1:nrow(pl)){
    ords = pl$order_ids[i]
    
    pos_ads = trimws(unlist(strsplit(ords, split = ',', fixed = T)))
    
    pos_ads = unique(ads[`wa_num` %in% c(pos_ads), .(agency, `Delivery Address` = address,`Delivery POC` = poc,`Delivery POC Phone number` =  phone, email)])
    
    if(nrow(pos_ads) != 1){
      
      if(length(unique(pos_ads[, `Delivery Address`]))== 1){
        pos_ads = pos_ads[1,]
      }else{
        warning(paste0(i,'\n',print_and_capture(pos_ads)))
        pos_ads = pos_ads[1,]
      }
    }
    
    pl[i, c('Address', 'POC', 'Phone', 'email') := pos_ads[, .(`Delivery Address`,`Delivery POC`,`Delivery POC Phone number`, email)]]
  }
  
  return(pl)
  
}
