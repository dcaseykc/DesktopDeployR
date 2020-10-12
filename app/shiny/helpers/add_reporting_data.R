#' Prepare the cycle data for analysis
#' @param fold directory path- location of the working directory
#' @param date cycle date
#' @param cycle_version numeric/character- version of the cycle. Usually increments up from 1. Failing to use a new value can result in files being overwritten (which is sometimes desirable)
#' @param ordersandtiers_version numeric/character- version of the orders and tiers files to use
add_reporting_data = function(fold, 
                        date,
                        cycle_version,
                        ordersandtiers_version){
  # debug
  # fold = 'C:/Users/alibrown/Documents/ppe/distribute_8-28-2020'
  # date = as.Date(c("2020-08-28"))
  # cycle_version = '2'
  # ordersandtiers_version = '2'
  
  #Load cycle information
  cycle_day = mday(date)
  cycle_mo = month(date)
  cycle_v = cycle_version
  
  ot_v = ordersandtiers_version
  suffix = paste0('_', cycle_mo,cycle_day,'_v', cycle_v)
  output = file.path(fold, suffix)

  tiering = setDT(load_spreadsheet(file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))))

  
  print(paste0('Reporting file updated - ',file.path(fold),'/',suffix))
} 
