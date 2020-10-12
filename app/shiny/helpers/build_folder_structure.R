#' Build the folder structure for a new round of stuff
#'
#' @param date date- From the \code{Sys.Date()} type input. Specifies the date of the cycle. Usually the Friday when Logs sends a roundup to the state
#' @param base_folder directory path- Location of the directory to create a cycle specific working folder
#' @return path of the working directory. It also creates a few folders.
build_folder_structure = function(date, base_folder){

  stopifnot('Base working folder directory does not exist. Please create it or point to an existing folder' = dir.exists(base_folder))

  folder_name = paste0('distribute_',month(date),'-',mday(date),'-', year(date))

  if(file.path(dirname(base_folder), folder_name) == base_folder){
    fold = file.path(dirname(base_folder), folder_name)
  }else{
    fold = file.path(base_folder, folder_name)
  }

  dir.create(fold)
  dir.create(file.path(fold, 'inv'))
  dir.create(file.path(fold, 'input_files'))

  return(fold)

}
