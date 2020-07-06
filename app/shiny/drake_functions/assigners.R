assign_ppe <- function(theitem, ppe, w, inv_each){

  print(paste0('Assigning: ', theitem))
  #subset the ppe to the relevant itemz
  r_ppe = ppe[itemz %in% theitem, ]
  
  #turned off to allow for blank returns
  #stopifnot(nrow(r_ppe)>=1)
  
  dist_me = inv_each[itemz %in% theitem]
  
  if(!all(r_ppe[, type] %in% w[, type])){
    stop(paste('Missing facility types in weights:', paste(setdiff(r_ppe[, type] , w[, type]), collapse = ', ')))
  }
  
  if(!all(r_ppe[, agency] %in% w[, agency])){
    stop(paste('Missing agency weights:', paste(setdiff(r_ppe[, agency] , w[, agency]), collapse = ', ')))
  }
  #some setup for the while loop
  iter = 0
  keep_going = TRUE
  
  r_ppe[, percent_filled := 0]
  r_ppe[, assigned := 0]
  
  #If there is nothing to assign, return that
  if(!(nrow(r_ppe[fill_me>0])>0) ){
    return(r_ppe)
  }
  
  #assign items algorithmically 
  while(keep_going){
    
    #how many products are left to distribute?
    remain = dist_me$available - r_ppe[, sum(assigned)]
    
    if(remain == 0 || iter > 10 || all(r_ppe[fill_me == 1,  percent_filled == 1])){
      keep_going = FALSE
    }else{
      print(paste(theitem, remain))
      #create weights
      w_sub= w[agency %in% r_ppe[percent_filled<1 & fill_me == 1, agency]]
      w_s1 = w_sub[, .(s1 = sum(chg)), by = type]
      w_s1[, s1 := s1/sum(s1)]
      
      w_s2 = copy(w_sub)
      w_s2[, s2 := w2/sum(w2), by = type]
      
      res = merge(w_s2, w_s1, by = 'type', all.x = T)
      res[, assign := remain * s1 * s2]
      
      #bad weights
      bw = res[is.na(s2) | is.na(s1)]
      
      if(nrow(bw)>0){
        stop(paste0(paste0(bw[, agency], collapse = ', '), ' have missing/0 weights'))
      }
      
      r_ppe = merge(r_ppe, res[, .(agency, type, assign)], by = c('agency', 'type'), all.x = T)
      r_ppe[!is.na(assign), assigned := assign + assigned]
      r_ppe[, assign := NULL]
      
      #get rid of fraction items
      r_ppe[, assigned := floor(assigned)]
      r_ppe[assigned > requested, assigned := requested]
      r_ppe[, percent_filled := assigned/requested]
      
    }
    
    iter = iter + 1 
    
  }
  
  remain = dist_me$available - r_ppe[, sum(assigned)]
  
  while(remain>0 && !all(r_ppe[fill_me == 1, percent_filled == 1])){
    r_ppe[sample(r_ppe[percent_filled <1 & fill_me == 1, .I],1), assigned := assigned + 1]
    r_ppe[, percent_filled := assigned/requested]
    remain = remain - 1
  }
  
  if(remain >0 & !all(r_ppe[fill_me == 1, percent_filled == 1])){
    stop('something broke')
  }
  remain = dist_me$available - r_ppe[, sum(assigned)]
  
  print(paste(remain, theitem, 'remaining after assignment'))
  
  return(r_ppe)
  
}
