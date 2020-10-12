print('run startup')
library('shiny')
library('shinydashboard')
library('openxlsx')
library('data.table')
library('tools')
library('xml2')
library('readxl')
library('drake')


print(getwd())

dir2look = ifelse(basename(getwd()) == 'shiny', 'helpers/', './app/shiny/helpers/')
for(fff in list.files(dir2look, full.names = TRUE)) source(fff) 

dir2look = ifelse(basename(getwd()) == 'shiny', 'drake_functions/', './app/shiny/drake_functions/')
for(fff in list.files(dir2look, full.names = TRUE)) source(fff)

dir2look = ifelse(basename(getwd()) == 'shiny', '../', './app')
defaults = setDT(read.csv(file.path(dir2look, 'defaults.csv'), stringsAsFactors = FALSE))
