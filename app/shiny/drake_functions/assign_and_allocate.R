# loadd(orders, inv, wt, cache = cache)
# w = copy(wt)
assign_and_allocate <- function(orders, inv, w, ltcf_categories, replacement_file = ""){

  #what tiers are we working with?
  looptiers = sort(unique(orders[,tier]))
  message(paste('Assign/Allocating for tiers:', paste0(looptiers, collapse = ', ')))
  
  #this will grown an object, which is naughty, but whatever
  holder = list()
  
  #orders = orders[item_type %in% 'sanitizer, hand- bulk']
  orders = orders[type %in% ltcf_categories, type := 'ltcf']
  
  for(ttt in looptiers){
    for(ppp in sort(unique(orders[tier == ttt, priority]), na.last = T)){
      message(paste(ttt, ppp))
      #remake inv_each each time
      inv_each = inv[, .(available = sum(ship_u * each_per_su)), by = itemz]
      
      valid = orders[tier %in% ttt & priority %in% ppp]
      
      ass = lapply(unique(valid[, itemz]), 
                   function(x) assign_ppe(x, valid, w, inv_each))
      
      if(ttt %in% c(1, 1.5)){
        alloc = lapply(ass, function(x) allocate_ppe(x, inv, 'greedy'))
       
      }else{
        alloc = lapply(ass, function(x) allocate_ppe(x, inv, 'spray'))
      }
      
      #draw down the inventory
      inv = drawdown_inv(alloc, inv)
      
      if(replacement_file != ''){
        message('running replacements')
        replacement = load_spreadsheet(replacement_file)
        
        for(id in unique(replacement[, grouping])){
          
          rrr = replacer(replace = replacement[grouping == id], alloc, inv, names(valid), w)
          
          alloc <- rrr[[1]]
          inv <- rrr[[2]]
          
        }
        
      }
      
      #add the allocations to the holder
      holder[[paste0(ttt,'_',ppp)]] <- alloc
      #ids = lapply(holder, function(x) vapply(x, function(y) unique(y[, itemz]), ""))
      
    }
  }
  
  #compile based on itemz
  ids = lapply(holder, function(x) vapply(x, function(y) unique(y$itemz), ""))
  itemz = unique(unlist(ids))
  allocations = lapply(itemz, function(x){
    res = lapply(holder, function(y){
      blah = vapply(y, function(z) unique(z$itemz), "")
      
      if(any(blah %in% x)){
        return(y[[which(blah %in% x)]])
      } else{
        return(data.table())
      }
    })
    
    res = rbindlist(res, fill = T)
  })
  
  
  return(allocations)
  
}