allocate_ppe = function(ord, inv, method = 'greedy', surplus_weight = .25, dnas){
  ord = copy(ord)
  print(paste0('Allocating: ', unique(ord[, itemz])))
  
  ord[, ord_id := .I]
  
  #subset inventory to match the itemz under consideration
  subinv = inv[itemz %in% ord[, itemz]]
  subinv[, items_left := ship_u]
  
  #start with the biggest order
  setorder(ord, -assigned)
  
  ord[, allocated := 0]
  
  if(nrow(subinv) == 0 ){
    return(ord)
  }
  
  if(method == 'greedy'){
    return(allocate_ppe_greedy(ord, subinv, dnas))
  }else if (method == 'spray'){
    return(allocate_ppe_spray(ord, subinv, dnas))
  }else{
    return(allocate_ppe_lp(ord, subinv, surplus_weight))
  }
}

#Ord: output from the assignment function.
#inv: inventory on hand
allocate_ppe_greedy = function(ord, subinv, donotallocate){
  
  for(iii in subinv[, Item_long]){
    ord[, (iii) := 0]
  }
  
  ord[, done := 0]
  
  #Use the greedy algorithm. Fill the biggest order and waterfall down.
  while(sum(subinv[, items_left])!=0 & !all(ord[, done == 1 ])){
    
    #choose the largest unfilled order
    tr = ord[done != 1, ord_id][1]
    tr = which(ord[, ord_id] %in% tr)
    
    subinv[, use_me :=1]
    if(ord[tr, agency] %in% donotallocate[, agency]){
      subinv = subinv[id %in% donotallocate[agency %in% ord[tr, agency], Item], use_me := 0]
    }
    
    #print(tr)
    #fill that order with whatever box causes the minimum deviation
    while(sum(subinv[use_me == 1, items_left])!=0 & ord[tr, assigned>allocated]){
      remain = ord[tr, assigned - allocated]
      this = subinv[items_left > 0 & use_me == 1, ][which.min(abs(remain - each_per_su)), item_id]
      this = which(subinv[, item_id] %in% this)
      ord[tr, allocated := subinv[this, each_per_su] + allocated]
      ord[tr, (subinv[this, Item_long]) := get(subinv[this, Item_long]) + 1 ]
      subinv[this, items_left := items_left - 1]
    }
    ord[tr, done := 1]
    subinv[,use_me := 1]
  }
  
  ord[, done := NULL]
  #make sure we don't acidentally distribute more than we have
  res = melt(ord[, lapply(.SD, sum), .SDcols = subinv[, Item_long]][, id:=.I], id.vars = 'id', variable.factor = FALSE)
  res[, id := NULL]
  res[, item_id := (tstrsplit(variable, split = ':', keep = 1))]
  res[, item_id := item_id]
  chk = merge(subinv, res, all.x = T, by = 'item_id')
  stopifnot(all(chk[, value <= ship_u]))
  return(ord)
}

allocate_ppe_spray = function(ord, subinv, donotallocate){
  
  for(iii in subinv[, Item_long]){
    ord[, (iii) := 0]
  }
  
  #Use the spray algorithm. starting at the biggest order, allocate a box at a time until we fill things, or run out of supply
  while(sum(subinv[, items_left])!=0 & !all(ord[, allocated>=assigned ])){
    
    for(ooo in ord[allocated < assigned, ord_id]){
      
      subinv[, use_me :=1]
      if(ord[ord_id %in% ooo, agency] %in% donotallocate[, agency]){
        subinv = subinv[id %in% donotallocate[agency %in% ord[ord_id %in% ooo, agency], Item], use_me := 0]
      }
      
      if(nrow(subinv[use_me == 1])>0 && sum(subinv[use_me == 1, items_left])==0){
        break
      }
      remain = ord[ord_id %in% ooo, assigned - allocated]
      this = subinv[items_left > 0 & use_me == 1, ][which.min(abs(remain - each_per_su)), item_id]
      ord[ord_id %in% ooo, allocated := subinv[item_id %in% this, each_per_su] + allocated]
      ord[ord_id %in% ooo, (subinv[item_id %in% this, Item_long]) :=  get(subinv[item_id %in% this, Item_long]) + 1]
      subinv[item_id %in% this, items_left := items_left - 1]
    }
  }
  
  #make sure we don't accidentally distribute more than we have
  res = melt(ord[, lapply(.SD, sum), .SDcols = subinv[, Item_long]][, id:=.I], id.vars = 'id', variable.factor = FALSE)
  res[, id := NULL]
  res[, item_id := (tstrsplit(variable, split = ':', keep = 1))]
  res[, item_id := item_id]
  chk = merge(subinv, res, all.x = T, by = 'item_id')
  stopifnot(all(chk[, value <= ship_u]))
  return(ord)
}


allocate_ppe_lp = function(ord, subinv, surplus_weight = .25){

  each_per_su = subinv[, each_per_su]
  n_box = subinv[, items_left]
  A = ord[assigned>0, assigned]
  A_id = ord[assigned>0, ord_id]
  box_id = subinv[, item_id]
  #start up a solver
  #number of boxes to solve for, and 2 additional constraints per facility 
  nconstraints = length(box_id) + 2*length(A_id)
  
  #number of boxes * number of facilities, plus a free variable
  ndvars = 1 + length(box_id) * length(A_id)
  
  my.lp = make.lp(nconstraints, ndvars)
  
  #set the names
  con_names = c(paste0('Box',box_id ), paste0('Shortage', A_id), paste0('Surplus', A_id))
  paster = function(x,y) { paste0('f', x, 'b', y)}
  dvar_names = as.vector(outer(A_id, box_id, paster))
  
  #order by facility id
  dvar_names = c('m', dvar_names[order(dvar_names)])
  dimnames(my.lp) <- list(con_names, dvar_names)
  
  #set the box columns to be integer
  set.type(my.lp, seq_len(length(dvar_names)-1)+1, 'integer')
  
  #construct a matrix to be fed into add.column interatively
  mat = matrix(0, nrow = nrow(my.lp), ncol = ncol(my.lp))
  
  dimnames(mat) <- list(con_names, dvar_names)
  
  #fill in the matrix for the summation constraints
  for(nb in seq_along(box_id)){
    rows = nb
    cols = which(dvar_names %in% paster(A_id, box_id[nb]))
    mat[nb, cols] <- 1
  }
  
  #fill in the matrix for shortage
  for(fac in A_id){
    
    #m's coeff is -1
    mat[paste0('Shortage', fac), 'm'] <- -1
    
    #coefs are the items per box divided by the target
    coefs = each_per_su/A[A_id %in% fac] * -1
    mat[paste0('Shortage', fac), paster(fac, box_id)] <- coefs
    
  }
  #fill in the matrix for surplus
  for(fac in A_id){
    
    #m's coeff is -1
    mat[paste0('Surplus', fac), 'm'] <- -1
    
    #coefs are the items per box divided by the target
    coefs = each_per_su/A[A_id %in% fac] * surplus_weight
    mat[paste0('Surplus', fac), paster(fac, box_id)] <- coefs
    
  }
  
  #fill in the columns
  for(i in seq_len(ncol(mat))){
    set.column(my.lp, i, mat[,i])
  }
  
  #set the objective
  set.objfn(my.lp, c(1, rep(0, ncol(mat)-1)))
  
  #set the constraints. All less than/equal to
  set.constr.type(my.lp, rep('<=', nrow(mat)))
  
  #set rhs
  set.rhs(my.lp, c(n_box, rep(-1, length(A_id)), rep(surplus_weight, length(A_id))))
  
  #solve
  solve(my.lp)
  
  #highlight results
  res = data.table(var = dvar_names, val = get.variables(my.lp))
  
  #remove m
  res = res[var != 'm', ]
  res = res[, c('fac', 'box_id') := tstrsplit(var, split = 'b', fixed ='T')]
  res[, fac := as.integer(gsub('f', "", fac, fixed = T))]
  res[, box_id := as.integer(gsub('b', "", box_id, fixed = T))]
  
  #merge on values
  res = merge(res, data.table(box_id = box_id, ipb = each_per_su, box_name = subinv[, Item_long]), all.x = T, by = 'box_id')
  
  #construct the results to be merged on to ord (long on facility, wide on box)
  res[, allocated := sum(val * ipb), by = fac]
  
  
  res = dcast(res[, .(ord_id = fac, allocated, val, box_name)], ord_id + allocated ~ box_name, value.var = 'val' )
  
  ord = merge(ord, res, all.x = T, by = 'ord_id')
  
  for(ccc in c('allocated', subinv[, Item_long])){
    ord[is.na(get(ccc)), (ccc) := 0]
  }
  
  return(ord)
  
}