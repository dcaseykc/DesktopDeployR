print('run app')
source('./app/shiny/startup.R')
# source('./app/shiny/ui.R')
# source('./app/shiny/server.R')
# app launching code, e.g.:
shiny::runApp('./app/shiny/', launch.browser=TRUE)
