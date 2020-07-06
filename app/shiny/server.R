print('run server')
server = function(input, output, session) {
  
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$wd <- renderText(getwd())
  output$sesh_deets = renderText(print_and_capture(sessionInfo()))
  output$Rhome = renderText(R.home())
  cache = reactiveValues()
  
  #build the folder when the button is clicked
  output$workfolder = renderText({
    req(input$go_validate)
    
    isolate({
      req(input$cycle_date, input$workdir)
      ret <- build_folder_structure(input$cycle_date, input$workdir)
      cache$workdir <- ret
    })
    
  })
  
  #Create the orders and tier files
  output$t_o <- renderText({
    req(input$ot_run, cache$workdir, input$cycle_date)
    isolate({
      order_and_tiers(fold = cache$workdir, date = input$cycle_date, t1 = input$ot_t1$datapath,
                      t2 = input$ot_t2$datapath, order_v = input$ot_ver, load_from_previous = (!is.null(input$ot_prev_ver) && !input$ot_prev_ver == ""),
                      prev_v = input$ot_prev_ver, previous_week = input$ot_prev_sum$datapath,
                      dump = input$ot_dump$datapath, add_fp = input$ot_add$datapath)
    })
  })
  
  #Create the item classifications file
  output$ic_o <- renderText({
    req(input$ic_old, input$ic_new, input$ic_go)
    isolate({
      if(is.null(cache$workdir)) return('Please load working directory options on the `Global Options tab` and try again')
      prep_item_classifications(cache$workdir, input$cycle_date, input$ic_old$datapath, input$ic_new)
    })
  })
  
  #Create the inventory
  output$inv_o <- renderText({
    req(input$inv_go)
    isolate({
      if(is.null(cache$workdir)) return('Please load working directory options on the `Global Options tab` and try again')
      mis_inv = which(is.null(c(input$inv_new, input$inv_new_ver, input$inv_old)))
      if(length(mis_inv)>0){
        return(paste0('Please provide valid information for: ', 
                      paste0(c('Version', 'New Inventory', 'Old Inventory')[mis_inv], collapse = ', ')))
      }
      prep_inventory(cache$workdir, input$cycle_date, input$inv_old$datapath, input$inv_new$datapath, input$inv_new_ver)
    })
  })
  
  #Hospital Data
  output$hosp_o <- renderText({
    req(input$hosp_go)
    isolate({
      if(is.null(cache$workdir)) return('Please load working directory options on the `Global Options tab` and try again')
      mis_hosp = which(is.null(c(input$hosp_inv$datapath, input$hosp_covid$datapath)))
      if(length(mis_hosp)>0){
        return(paste0('Please provide valid information for: ', 
                      paste0(c('Inventory', 'Covid')[mis_hosp], collapse = ', ')))
      }
      prep_hospital_data(cache$workdir, input$hosp_inv$datapath, input$hosp_covid$datapath)
    })
  })
  
  #for the other inputs tab, copy the files to the new folder
  output$oi_o <- renderText({
    req(input$oi_go)
    isolate({
      if(is.null(cache$workdir)) return('Please load working directory options on the `Global Options tab` and try again')
      mis_oi = which(is.null(c(input$linelist$datapath, input$cw$datapath, input$replacements$datapath, input$acrciq$datapath, input$chgs$datapath)))
      if(length(mis_oi)>0){
        return(paste0('Please provide valid information for: ', 
                      paste0(c('linelist', 'crosswalk', 'replacement', 'acrciq', 'chgs')[mis_oi], collapse = ', ')))
      }
      copy_other_inputs(cache$workdir, input$linelist$datapath, input$cw$datapath,
                        input$replacements$datapath, input$acrciq$datapath,
                        input$chgs$datapath, input$cache_folder)
    })
  })
  #run allocations
  output$alloc_response <- renderText({
    
    if(input$make_allocs == 0) return('Press button to run allocations')
    isolate({
      if(is.null(cache$workdir)) return('Please load working directory options on the `Global Options tab`')
      
      #save the things so its easier to debug
      hold_cache = reactiveValuesToList(cache)
      hold_input = reactiveValuesToList(input)
      save(hold_cache, hold_input, file = file.path(cache$workdir, 'most_recent_inputs.Rdata'))
      run_allocations_drake(fold = cache$workdir,
                            date = input$cycle_date,
                            cycle_version = input$cycle_v,
                            inventory_version = input$inv_v,
                            ordersandtiers_version = input$ot_v,
                            runtiers = input$runtiers,
                            sized_items = input$sized,
                            ignore_me = input$ignore_me,
                            standardize_chinook = TRUE,
                            holdback_frac = input$holdback_frac,
                            hosp_supply = input$hosp_thresh)
      
      'Complete'
      
      
    })

  })
  
}
