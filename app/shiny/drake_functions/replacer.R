#' A function to do replacements and swaps
#' 
#' 
#' @param replace a data.table of instructions on how to replace
#' @param alloc list of allocations to be added to
#' @param inv data.table of inventory after the allocations of allocs has been draw down
#' @param validnames character vector of names from the valid data.table

replacer = function(replace, alloc, inv, validnames, w, donotallocate){
  
  stopifnot(length(unique(replace[,grouping]))== 1)
  stopifnot(length(unique(replace[,ratio_old_new]))== 1)
  stopifnot(length(unique(replace[,old_item_type]))== 1)
  stopifnot(length(unique(replace[,old_item_size]))== 1)
  
  #check the allocations to see if this is necessary, otherwise just return the allocations
  allocz = rbindlist(alloc, fill = T)
  
  sub_allocz = allocz[item_type %in% replace[, old_item_type]]
  
  if(!all(is.na(replace[, old_item_size]))){
    sub_allocz = sub_allocz[size %in% replace[, old_item_size]]
  }
  
  #any unfilled orders?
  remain = sub_allocz[allocated < requested & fill_me != 0, ]
  
  #if so, try to do swaps
  if(nrow(remain)>0){
    
    remain = remain[, .SD, .SDcols = c(validnames, 'allocated')]
    sub_inv = inv[item_type %in% replace[, new_item_type]]
    
    if(!all(replace[, is.na(new_item_size)])){
      sub_inv = sub_inv[size %in% replace[, new_item_size]]
    }
    
    #adjust remain and sub inv so the item info lines up
    remain[, c('item_type', 'size', 'requested') :=
             .(paste0('replacement: ', unique(replace[, old_item_type])),
               unique(replace[, old_item_size]),
               ceiling((requested - allocated)))]
    remain[, itemz := paste0(item_type, ', ', size)]
    
    sub_inv[, c('item_type', 'size') :=
              .(paste0('replacement: ', unique(replace[, old_item_type])),
                unique(replace[, old_item_size]))]
    sub_inv[, itemz := paste0(item_type, ', ', size)]
    
    #collapse requests
    remain[is.na(droppies), droppies := 0]
    remain = remain[, .(requested = sum(requested), fill_me = max(fill_me, na.rm = T), droppies = max(droppies, na.rm = T),
                        ppe_id = paste0(paste0(ppe_id, collapse = ','), ',replacement')),
                    by = .(agency, order_ids, itemz,item_type, type, tier, priority, lnum)]
    remain[droppies == 0, droppies := NA]
    
    sub_inv_each = sub_inv[, .(available = sum(ship_u * each_per_su)), by = itemz]
    
    #change the requests based on the ratio
    remain[, requested := requested * unique(replace[, ratio_old_new])]
    remain[fill_me != 0, fill_me := 1]
    #Assign and allocate the replacements
    rass = lapply(unique(remain[, itemz]), 
                  function(x) assign_ppe(x, remain, w, sub_inv_each))
    ralloc = lapply(rass, function(x) allocate_ppe(x, sub_inv, 'spray', dnas = donotallocate))
    
    ralloc = ralloc[[1]]
    its = unique(inv[Item_long %in% names(ralloc), itemz])
    
    #for each item type assigned, either add new rows, or up the amounts of other ones
    for(it in its){
      base_names = setdiff(names(ralloc), inv[,Item_long])
      var_names = intersect(names(ralloc), inv[itemz == it, Item_long])
      alloc_id = which(vapply(alloc, function(x) unique(x[, itemz]), "") %in% it)
      
      adder = copy(ralloc)[, .SD, .SDcols = c(base_names, var_names)][, keep := rowSums(.SD)>0,.SDcols = var_names][keep==T,][,keep:=NULL]
      
      #make requested, assigned, and allocated all the same (because their numbers at this point, reflect og orders)
      adder[, c('requested', 'assigned', 'allocated') := 0]
      for(vvv in var_names){
        adder[, c('requested', 'assigned', 'allocated') := lapply(.SD, function(x) x + get(vvv) * sub_inv[Item_long == vvv, each_per_su]), .SDcols = c('requested', 'assigned', 'allocated')]
      }
      
      adder[, itemz:=it]
      adder[, size := unique(inv[itemz == it, size])]
      adder[, item_type := unique(inv[itemz == it, item_type])]
      
      if(length(alloc_id)>0){
        adder = rbind(alloc[[alloc_id]], adder, fill = T)
      }
      if(nrow(adder)>0){
        
        #collapse
        col_cols = c('requested', 'assigned', 'allocated', var_names, 'fill_me', 'droppies', 'ord_id', 'ppe_id')
        adder = adder[, lapply(col_cols, function(x){
          if(x %in% c('ord_id', 'ppe_id')){
            return(paste0(get(x), collapse = ' | '))
          }else if(x %in% c('fill_me', 'droppies')){
            blah = unique(get(x))
            stopifnot('fill_me or droppies gives inconsitent information' = length(blah) <= 1)
            return(blah)
          }else{
            return(sum(get(x)))
          }
        }),
        by = .(agency, type, order_ids, itemz, item_type, size, tier, priority, lnum)]
        
        setnames(adder, paste0('V', seq_along(col_cols)), col_cols)
        adder[, percent_filled := allocated/requested]
        
        if(length(alloc_id)>0){
          alloc[[alloc_id]] <- adder
        }else{
          alloc = append(alloc, list(adder))
        }
        
      }
    }
    
    
    #draw down coveralls
    inv = drawdown_inv(ralloc, inv)
  }
  
  return(list(alloc, inv))
  
  
  
}