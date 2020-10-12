#' Prepare the hospital data for analysis
#' @param fold directory path- location of the working directory
#' @param ppe file path- location of the hospital PPE report/numbers downloaded from WA health
#' @param covid file path- location of the hospital COVID inpatient numbers downloaded from WA health
#' @return file path to the saved data.
prep_hospital_data = function(fold, ppe, covid){
  ppe_hosp = setDT(read_excel(ppe, skip = 1))
  covid_hosp = setDT(read_excel(covid, skip = 1))
  
  #Convert the WA health names into PHSKC item type classifications
  items = data.table(`Supply Type` = c("Eye Protection", "Gloves", "Gowns (Single Use)", "N-95", "Surgical Masks", 
                                       "Ventilator supplies (disposables)", "Other respirators Including PAPRs", 
                                       "Face Shields", "Simple Masks", "CAPR Shields", "Goggles/Glasses", 
                                       "Masks with Shields"),
                     item_type = c('goggles', 'non-latex gloves', 'gowns', 'N95', 'mask', 'ventilator',
                                   'papr', 'shield, full face', 'mask', 'capr', 'goggles', 'shield, full face'))
  ppe_hosp = merge(ppe_hosp, items, all.x = T, by = 'Supply Type' )
  
  #Convert the on hand supply categories into days. Use the first day of the period to be conservative
  sup = data.table(`On hand Supply` = c("0", "15+", "7-14", "4-6", "1-3"), days = c(0,15,7,4,1)) #be conservative
  ppe_hosp = merge(ppe_hosp, sup, by = 'On hand Supply', all.x = T)
  stopifnot('Missing days' = all(!is.na(ppe_hosp[,days])))
  stopifnot('Missing item classifications' = all(!is.na(ppe_hosp[,item_type])))
  
  #keep the latest entry
  ppe_hosp = ppe_hosp[ppe_hosp[, .I[which.max(`Created On Date only`)], by = .(name = `'Supply Entry'[Facility]`, item_type)]$V1]
  
  #collapse and create a UW w/o Valley medical row because of UW reasons
  ppe_hosp = ppe_hosp[, .(days = min(days, na.rm = T)), by = .(name = `'Supply Entry'[Facility]`, item_type)]
  uw = ppe_hosp[grepl('UW Medicine', name, fixed = T) & !grepl("Valley", name), .(days = min(days, na.rm = T)), by = 'item_type']
  uw[, name := 'UW Medicine']
  ppe_hosp = rbind(ppe_hosp, uw)
  
  #covid per hospital
  covid_hosp = covid_hosp[covid_hosp[, .I[which.max(`Created On`)], by = Facility]$V1,]
  covid_hosp = covid_hosp[, .(name = `Facility`, covid = `Sum of COVID PUI` + `COVID Confirmed`)]
  
  #collapse to UW medicine
  uw = covid_hosp[grepl('UW Medicine', name, fixed = T) & !grepl("Valley", name), .(covid = sum(covid))]
  uw[, name := 'UW Medicine']
  covid_hosp = rbind(covid_hosp[, .(name, covid)], uw)
  
  hosp = rbind(ppe_hosp[!is.na(item_type), .(value = days, name, item_type)], covid_hosp[, .(name, value = covid, item_type = 'covid')])
  
  hosp = dcast(hosp, name ~ item_type, value.var = 'value')
  
  write.csv(hosp, file = file.path(fold, paste0('hospital_data.csv')), row.names = F)
  return(file.path(fold, paste0('hospital_data.csv')))
}