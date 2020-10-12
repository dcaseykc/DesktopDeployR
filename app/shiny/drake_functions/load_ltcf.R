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
load_ltcf_data <- function(ppe, ltcf_categories, residents, beds, cases){

  #load things
  residents = fix_ltcf_lnums(load_spreadsheet(residents), 'License.number', 'Facility.Name')
  cases_ltcf = load_spreadsheet(cases)
  
  beds = fix_ltcf_lnums(load_spreadsheet(beds), 'LicenseNumber', 'FacilityName')

  bts = data.table(abbriv = c('AL', 'NF', 'AF'), type = c('Assisted Living Facility', 'Nursing Home', 'Adult Family Home'))

  #fix up the beds file
  residents = residents[Provider.County == 'King', .(lnum = License.number, res = as.numeric(`#.of.Residents`), type = `Provider.Type`, bedbackup = as.numeric(`Bed.Count`))][!is.na(type), ]
  beds = unique(beds[, .(lnum = LicenseNumber,abbriv = trimws(FacilityType), nbeds = LicensedBedCount)])

  beds = merge(beds, bts, all.x = T, by = 'abbriv')

  residents = merge(residents, beds[,.(lnum, nbeds, type)], by = c('lnum', 'type'), all = T)

  baddies = residents[, .N, lnum][N>1, lnum]
  if(length(baddies)>0) stop(paste0('lnum:agency relationship is not 1:1 for ', paste0(baddies, ', ')))

  #'License Number', 'Facility Name', 'Facility Type', 'Status', 'Count'
  cases_ltcf = cases_ltcf[, .(License = `License Number`, Name = `Facility Name`, Type = 'Facility Type', Status, Count = as.numeric(Count))]
  cases_ltcf = cases_ltcf[, .(cases = sum(Count)), by = .(License, Name, Type, Status)]
  
  for(ag in unique(ppe[type %in% ltcf_categories, agency])){
    nums = unique(ppe[agency ==ag, lnum])
    if(length(nums)!=1 || all(is.na(nums))){
      stop(paste0(ag, ': License is either duplicated by agency or missing'))
    }

    nums = trimws(unlist(strsplit(nums, ',', fixed = T)),whitespace =  "[\\h\\v]")

    residents[lnum %in% nums, agency := ag]
    #beds[lnum %in% nums, agency := ag]
    cases_ltcf[License %in% nums, agency := ag]
  }

  #check to make sure we have all the data
  if(nrow(unique(ppe[type %in% ltcf_categories, ]))>0){
    chk = setdiff(unique(ppe[type %in% ltcf_categories, agency]), residents[,agency])
    if(length(chk)>0) stop(paste0('Could not find the following agencies:', paste0(chk, collapse = ', '),
                                  '. Check to make sure it has a license number that is unique within the tier list.
                                  In some rare cases, the facility might not be represented in the comprehensive LTCF lists.
                                  Check DSHS website for more.'))

    #bed weight var-- preferably, residents, if not, beds
    residents = residents[!is.na(agency), .(res = sum(res, na.rm = T),
                                            nbeds = sum(nbeds, na.rm = T),
                                            bedbackup = sum(bedbackup, na.rm = T),
                                            type = paste(unique(type), collapse =',')), agency]


    #if res > nbeds, cap at beds
    residents[, wt := res]
    residents[is.na(wt) | wt ==0, wt := nbeds]
    residents[is.na(wt) | wt ==0, wt := bedbackup]
    residents[is.na(wt) | wt ==0 & type == 'Adult Family Home', wt := 5]

    if(!all(!is.na(residents[, wt]))){
      stop(paste('The following agencies have an NA weight (either beds or resident counts are NA):',
                 paste0(residents[is.na(wt), agency], collapse = ', ')))
    }

    stopifnot(all(!is.na(residents[, wt])))
    stopifnot(all(residents[,wt>0]))

    #capture the cases
    cases_ltcf = cases_ltcf[!is.na(agency), .(cases = sum(cases)), by = .(agency,Status,License)]
    cases_ltcf = cases_ltcf[cases>0 & Status == 'Active' & License != '']

    residents = merge(residents, cases_ltcf, all.x = T, by = 'agency')

    stopifnot(all(unique(ppe[type %in% ltcf_categories, agency]) %in% residents[, agency]))
    residents = residents[, .(agency, nbeds = wt, cases)]

  }else{
    residents = data.table(agency = "", nbeds = 1, cases = 1)
    residents = residents[agency %in% 'bah']
  }

  return(residents)
}
