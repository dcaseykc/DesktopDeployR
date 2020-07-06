#' Load and format ltcf data for allocations and assignments
#' 
#' @param ppe data.table. Output of the load_ppe function
#' @param ltcf_categories character. (facility) type(s) that make up ltcfs
#' @param residents file.path. Filepath with info on N residents in a facility
#' @param beds file path. Filepath with information with beds by license number
#' @param cases file path. Filepath with information on cases (from CDEPIs db)
#' @param crosswalk file path. Filepath to the cddb and license number crosswalk
#' 
#' @return a data.table with a row per ltcf agency, number of residents/beds, and cases
#' 

# ppe = readd(ppe, cache = cache)
load_ltcf_data <- function(ppe, ltcf_categories, residents, beds, cases, cw){
  
  #load things
  residents = fix_ltcf_lnums(load_spreadsheet(residents), 'License.number', 'Facility.Name')
  cases_ltcf = load_spreadsheet(cases)
  cw = fix_ltcf_lnums(load_spreadsheet(cw), 'LicenseNumber', 'FacilityName')
  beds = fix_ltcf_lnums(load_spreadsheet(beds), 'LicenseNumber', 'FacilityName')
  
  #fix up the beds file
  residents = residents[, .(lnum = License.number, res = as.numeric(`#.of.Residents`), type = `Provider.Type`)][!is.na(type), ]
  beds = unique(beds[LicenseNumber %in% residents[, lnum] , .(lnum = LicenseNumber,abbriv = trimws(FacilityType), nbeds = LicensedBedCount)])
  
  cases_ltcf = cases_ltcf[, .(DBID, res_dead = as.numeric(`Res Cnt Death`), res_hosp = as.numeric(`Res Cnt Hsp`),
                              res_sym = as.numeric(`Res Cnt Sym`), res_pos = as.numeric(`Res Test Pos`),
                              classif = `Classification Value`)]
  
  
  cases_ltcf[, c('res_dead', 'res_sym', 'res_pos', 'res_hosp') := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = c('res_dead', 'res_sym', 'res_pos', 'res_hosp')]
  cases_ltcf[, here := ifelse(res_sym>res_pos, res_sym, res_pos)]
  cases_ltcf[, left := ifelse(res_dead>res_hosp, res_dead, res_hosp)]
  cases_ltcf[, cases := ifelse((here-left) > 0, here-left, 0)]
  cases_ltcf[, res := res_sym + res_pos + res_hosp]
  cases_ltcf = cases_ltcf[, .(cases = sum(cases)), by = .(DBID, res)]
  cases_ltcf = merge(cases_ltcf, cw, by = 'DBID')
  
  
  for(ag in unique(ppe[type %in% ltcf_categories, agency])){
    nums = unique(ppe[agency ==ag, lnum])
    if(length(nums)!=1 || all(is.na(nums))){
      stop(paste0(ag, ': License is either duplicated by agency or missing'))
    }
    
    nums = trimws(unlist(strsplit(nums, ',', fixed = T)),whitespace =  "[\\h\\v]")
    
    residents[lnum %in% nums, agency := ag]
    beds[lnum %in% nums, agency := ag]
    cases_ltcf[LicenseNumber %in% nums, agency := ag]
    
  }
  
  #check to make sure we have all the data
  if(nrow(unique(ppe[type %in% ltcf_categories, ]))>0){
    chk = setdiff(unique(ppe[type %in% ltcf_categories, agency]), residents[,agency])
    if(length(chk)>0) stop(paste0('Could not find the following agencies:', paste0(chk, collapse = ', '),
                                  'Check to make sure it has a license number that is unique within the tier list.
                                  In some rare cases, the facility might not be represented in the comprehensive LTCF lists.
                                  Check DSHS website for more.'))
  
    #bed weight var-- preferably, residents, if not, beds
    residents = residents[!is.na(agency), .(res = sum(res, na.rm = T), type = paste(unique(type), collapse =',')), agency]
    beds = beds[, .(nbeds = sum(nbeds)), agency]
    st = nrow(residents)
    residents = merge(residents, beds, all.x = T, by = c('agency'))
    stopifnot(nrow(residents)==st)
    residents[, frac_filled := res/nbeds]
    
    #if res > nbeds, cap at beds
    residents[, wt := res]
    residents[res >nbeds, wt := nbeds]
    residents[is.na(wt), wt := nbeds]
    residents[wt %in% 0, wt := nbeds]
    residents[is.na(wt) & type == 'Adult Family Home', wt := 5]
    
    if(!all(!is.na(residents[, wt]))){
      stop(paste('The following agencies have an NA weight (either beds or resident counts are NA):',
                 paste0(residents[is.na(wt), agency], collapse = ', ')))
    }
    
    stopifnot(all(!is.na(residents[, wt])))
    stopifnot(all(residents[,wt>0]))
    
    #capture the cases
    cases_ltcf = cases_ltcf[!is.na(agency), .(cases = sum(cases)), by = agency]
    cases_ltcf = cases_ltcf[cases>0]
    
    residents = merge(residents, cases_ltcf, all.x = T, by = 'agency')
    
    stopifnot(all(unique(ppe[type %in% ltcf_categories, agency]) %in% residents[, agency]))
    residents = residents[, .(agency, nbeds = wt, cases)]
    
  }else{
    residents = data.table(agency = "", nbeds = 1, cases = 1)
    residents = residents[agency %in% 'bah']
  }
  
  return(residents)
}
