# alloc <- readd(allocations, cache = cache)
# alloc = holder[[length(holder) -1]]
# sani_inv = readd(inv, cache = cache)

bulk_to_bottle = function(sani_alloc, sani_inv, w){
  
  ids = vapply(sani_alloc, function(x) unique(x[, itemz]), "")
  
  bulk_id = which(ids %in% 'sanitizer, hand- bulk')
  bottle_id = which(ids %in% 'sanitizer, hand- bottle')
  
  sani_inv = sani_inv[item_type %in% paste0(c('sanitizer, hand- '), c('bulk','bottle'))]
  sani_inv_each = sani_inv[, .(available = sum(ship_u * each_per_su)), by = itemz]
  
  #for now, just replace bulk with bottle
  if(length(bulk_id)>0){
    if(any(sani_alloc[[bulk_id]][fill_me != 0, requested>allocated])>0){
      # browser()
      it = 'sanitizer, hand- bottle'
      need_sani = copy(sani_alloc[[bulk_id]][fill_me != 0 & requested>allocated])
      
      #clean up
      need_sani[, grep('C19-', names(need_sani), value = T, fixed = T) := NULL]
      #to do-- figure out a way to make this smarter for bottles of different sizes. for now make the inflation factor a little smaller to account for different sized bottles 
      need_sani[, requested := round((requested - allocated)/.1175)] #convert into 16.9 oz bottles from gallons
      need_sani[, c('percent_filled', 'assigned', 'ord_id', 'allocated') := NULL]
      need_sani[, c('itemz', 'item_type') := 'sanitizer, hand- bottle']
      
      #assign
      ass_sani = assign_ppe('sanitizer, hand- bottle', need_sani, w, sani_inv_each)
      alloc_sani = allocate_ppe(ass_sani, sani_inv, method = 'spray')
     
      base_names = setdiff(names(alloc_sani), sani_inv[,Item_long])
      var_names = intersect(names(alloc_sani), sani_inv[itemz == 'sanitizer, hand- bottle', Item_long])
      alloc_id = bottle_id
      
      adder = copy(alloc_sani)[, .SD, .SDcols = c(base_names, var_names)][, keep := rowSums(.SD)>0,.SDcols = var_names][keep==T,][,keep:=NULL]
      
      #make requested, assigned, and allocated all the same (because their numbers at this point, reflect og orders)
      adder[, c('requested', 'assigned', 'allocated') := 0]
      for(vvv in var_names){
        adder[, c('requested', 'assigned', 'allocated') := lapply(.SD, function(x) x + get(vvv) * sani_inv[Item_long == vvv, each_per_su]), .SDcols = c('requested', 'assigned', 'allocated')]
      }
      
      adder[, itemz:=it]
      adder[, size := unique(sani_inv[itemz == it, size])]
      
      
      if(length(alloc_id)>0){
        adder = rbind(sani_alloc[[alloc_id]], adder, fill = T)
      }
      
      if(nrow(adder)>0){
        #collapse
        col_cols = c('requested', 'assigned', 'allocated', var_names, 'fill_me', 'droppies', 'ord_id', 'ppe_id')
        adder = adder[, lapply(col_cols, function(x){
          if(x %in% c('ord_id', 'ppe_id')){
            return(paste0(get(x), collapse = ' | '))
          }else if(x %in% c('fill_me', 'droppies')){
            blah = unique(get(x))
            stopifnot(length(blah) <= 1)
            return(blah)
          }else{
            return(sum(get(x)))
          }
        }),
        by = .(agency, type, order_ids, itemz, item_type, size, tier, priority, lnum)]
        
        setnames(adder, paste0('V', seq_along(col_cols)), col_cols)
        adder[, percent_filled := allocated/requested]
        
        if(length(alloc_id)>0){
          sani_alloc[[alloc_id]] <- adder
        }else{
          sani_alloc = append(sani_alloc, list(adder))
        }
      }
    }
  }
  if(!exists('alloc_sani')) alloc_sani = data.table()
  
  return(list(sani_alloc, alloc_sani))
}
