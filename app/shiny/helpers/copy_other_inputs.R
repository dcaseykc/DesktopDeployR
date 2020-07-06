#' Copy and standardize a few misc inputs
#' @param fold directory path- location of the working directory for the cycle
#' @param linelist file path- location of the CDEPI line list of covid cases
#' @param crosswalk file path- location of the CDEPI crosswalk between DBIDs and DSHS license numbers
#' @param replace file path- location of the replacement instructions
#' @param acrciq file path- location of the acrc/iq occupancy numbers
#' @param chgs file path- location of the number of PPE changes per tier 1 category
#' @param cache_folder file path- location of where the drake cache should be created (as a subfolder)
copy_other_inputs <- function(fold, linelist, crosswalk, replace, acrciq, chgs, cache_folder){
  stopifnot('cache folder does not exist' = dir.exists(cache_folder))
  
  #Copy and standardize naming for misc files
  file.copy(crosswalk, file.path(fold, paste0('crosswalk.', file_ext(crosswalk))))
  file.copy(replace, file.path(fold, paste0('replacements.', file_ext(replace))))
  file.copy(acrciq, file.path(fold, paste0('acrciq.', file_ext(acrciq))))
  file.copy(chgs, file.path(fold, paste0('chgs.', file_ext(chgs))))
  
  #the line list can sometimes have sensitive data so this to reduce the chance of data leakage
  linelist = load_spreadsheet(linelist)
  setnames(linelist, tolower(names(linelist)))
  linelist = linelist[, .(dbid, `res cnt death`, `res cnt hsp`,
                 `res cnt sym`, `res test pos`,`classification value`)]
  setnames(linelist, c('DBID', 'Res Cnt Death', 'Res Cnt Hsp', 'Res Cnt Sym', 'Res Test Pos', 'Classification Value'))
  
  write.csv(linelist, row.names = F, file.path(fold, 'linelist.csv'))
  
  #make a little csv that contains where the path will be.
  write.csv(data.frame(cache_folder_path = cache_folder), row.names = F, file.path(fold, 'cache_loc.csv'))
  
  return('Updates & Uploads complete')
  
  
}