fix_ltcf_lnums = function(dt, lname, fname){
  
  dt = copy(dt)
  dt[, (lname) := as.character(get(lname))]
  # dt[, (lname) := NULL]
  # setnames(dt, '__blah', lname)
  
  dt[(grepl(1413, dt[, get(lname)], fixed = T) & grepl('river', dt[, tolower(get(fname))], fixed = T)), 
     (lname) := gsub(1413, '1413_2', get(lname), fixed = T)]
  
  dt[(grepl(1413, dt[, get(lname)], fixed = T) & grepl('briar', dt[, tolower(get(fname))], fixed = T)), 
     (lname) := gsub(1413, '1413_1',get(lname), fixed = T)]
  
  dt[(grepl(1532, dt[, get(lname)], fixed = T) & grepl('park', dt[, tolower(get(fname))], fixed = T)), 
     (lname) := gsub(1532, '1532_1', get(lname),fixed = T)]
  dt[(grepl(1532, dt[, get(lname)], fixed = T) & grepl('spring', dt[, tolower(get(fname))], fixed = T)), 
     (lname) := gsub(1532, '1532_2', get(lname), fixed = T)]
  
  dt[, (lname) := trimws(get(lname), whitespace = "[\\h\\v]")]
  
  return(dt)
  
}
