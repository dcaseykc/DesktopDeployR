load_spreadsheet = function(fp){
  if(file_ext(fp) == 'csv'){
    sp = fread(fp)
  }else{
    sp = read.xlsx(fp)
    setDT(sp)
  }
  
  return(sp)
  
}

print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}