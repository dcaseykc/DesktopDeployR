compile_user_functions = function(){
  funky = ls(envir = .GlobalEnv)
  
  classy = vapply(funky, function(x) is.function(get(x, envir = .GlobalEnv)), T)
  
  return(mget(funky[classy], envir = .GlobalEnv))
}
