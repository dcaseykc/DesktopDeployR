#' Load a prep hospital data
#' @param hosp file path to the weekly report of hospital data
#' 
#' @return a list- list(hosp_items, hosp_cases)
#hosp = "C:/Users/dcasey/OneDrive - King County/covid/PPE disbursement/distribute_4-24_order/Weekly Aggregate Report - King_0430_1430.xlsx"

load_hospital_data = function(hosp, threshold){
  hosp = load_spreadsheet(hosp)
  #setorder(hosp, Date)
  #hosp = hosp[, lapply(.SD, function(x) last(na.omit(x))), by = c('Facility'), .SDcols = c(grep('Days', names(hosp), value = T), 'Patient.Confirmed.or.Suspected', 'Total.Beds', 'Total.ICU.Beds')]
  #setnames(hosp, names(hosp), gsub(' ', '.', names(hosp), fixed = T))
  #create UW
 # hosp[grepl('UW Medicine', Facility) & ! Facility %in% c('UW Medicine/Valley Medical Center'), Facility := 'UW Medicine']
  
  names2 = setdiff(names(hosp), 'name')
  hosp = hosp[, (names2) := lapply(.SD, round), .SDcols = names2]
  
  # find out which item_types have over 10 days supply and ignore those requests
  hosp_items = melt(hosp,id.vars = 'name', variable.factor = FALSE, variable.name = 'item_type')
  hosp_items = hosp_items[value > threshold & item_type != 'covid', droppies := 1]

  hosp_cases = hosp[, .(name, covid)]
  hosp_cases[covid == 0, covid := 1]
  
  return(list(hosp_items = hosp_items, hosp_cases = hosp_cases))
  
  
}