wd = 'C:/Users/dcasey/OneDrive - King County/covid/PPE disbursement/distribute_7-10-2020'
load(file.path(wd ,'most_recent_inputs.Rdata'))
setwd('app/shiny')
cache = hold_cache
input = hold_input
fold = cache$workdir
date = input$cycle_date
cycle_version = input$cycle_v
inventory_version = input$inv_v
ordersandtiers_version = input$ot_v
runtiers = input$runtiers
sized_items = input$sized
ignore_me = input$ignore_me
standardize_chinook = TRUE
holdback_frac = input$holdback_frac
hosp_supply = input$hosp_thresh
n95except = input$n95except
cache_loc =input$cache_folder
