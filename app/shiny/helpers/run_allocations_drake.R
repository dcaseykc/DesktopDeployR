#' Manages and excutes the drake process
#' @param fold directory path- working folder
#' @param date cycle date
#' @param cycle_version numeric/character- version of the cyle. Usually increments up from 1. Failing to use a new value can result in files being overwritten (which is sometimes desirable)
#' @param inventory_version numeric/character- version of the inventory to use for this run
#' @param ordersandtiers_version numeric/character- version of the orders and tiers files to use
#' @param runtiers character as a `;` separated list- Which tiers (as found in the tiers file column current.tier) should be run? Note, tiers are run from lowest to highest in a dependent-iterative fashion.
#' @param sized_items character as a `;` separated list- item_type(s) that should be allocated by size
#' @param ignore_me character as a `;` separated list- item_type(s) that should not be allocated at all
#' @param standardize_chinook logical- should all addresses that are likely going to chinook to be standardized (e.g. sent to the first floor)
#' @param holdback_frac numeric value between 0 and 100. Signifies the percent of supplies of any given item that can be allocated.
#' @param hosp_supply numeric value. Number of days of supply a hospital must have less than to be eligible for a shipment. Is calculated per item type.
run_allocations_drake <- function(
                      fold,
                      date,
                      cycle_version,
                      inventory_version,
                      ordersandtiers_version,
                      runtiers,
                      sized_items,
                      ignore_me = "",
                      standardize_chinook = T,
                      holdback_frac = 95,
                      hosp_supply = Inf,
                      n95except = "",
                      cache_loc){

  stopifnot('Parent folder for drake cache does not exist' = !missing(cache_loc) && dir.exists(cache_loc))

  #fix some inputs
  runtiers = trimws(unlist(strsplit(runtiers, ';', fixed = TRUE)))
  sized_items = trimws(unlist(strsplit(sized_items, ';', fixed = TRUE)))
  ignore_me = trimws(unlist(strsplit(ignore_me, ';', fixed = TRUE)))
  n95except = trimws(unlist(strsplit(n95except, ';', fixed = TRUE)))
  holdback_frac = as.numeric(holdback_frac)
  if(is.na(holdback_frac) || holdback_frac >100 || holdback_frac<0){
    stop('Hold back percentage must be between 0 and 100')
  }
  holdback_frac = holdback_frac/100
  standardize_chinook_addresses = as.logical(standardize_chinook)
  stopifnot('standardize chinook must be a logical' = !is.na(standardize_chinook_addresses))
  hosp_supply = as.numeric(hosp_supply)
  stopifnot('Hospital supply must be greater than 0' = hosp_supply>0)

  #Load cycle information
  cycle_day = mday(date)
  cycle_mo = month(date)
  cycle_v = cycle_version
  inv_v = inventory_version
  ot_v = ordersandtiers_version
  suffix = paste0('_', cycle_mo,cycle_day,'_v', cycle_v)
  output = file.path(fold, suffix)
  dir.create(output)

  #construct cache
  dir.create(file.path(cache_loc, suffix))

  if (!dir.exists(file.path(cache_loc, suffix, '.drake')))
    invisible(new_cache(path = file.path(cache_loc, suffix, '.drake')))

  cache = drake_cache(file.path(cache_loc, suffix, '.drake'))

  #governing variables
  ltcf_categories = c('snf + alf', 'afh', 'supported living', 'alf', 'snf', 'ltcf')

  #routes by region
  regions = c('north_seattle_shoreline','bellevue','sw_king_county','east_king_county','renton','south_seattle_downtown','se_king_county')

  #Inputs! file paths
  template = file.path('./templates/template_order_87.xlsx')
  tiering = file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))
  orders = file.path(fold, paste0('order_list_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))
  item_class = file.path(fold, "item_classifications.csv")
  replacement_file = file.path(fold, 'replacements.xlsx')
  inv_fp = file.path(fold, 'inv', paste0('inventory_',cycle_mo, cycle_day, '_', inv_v,'.csv'))
  hosp = file.path(fold, 'hospital_data.csv')
  residents <- file.path('templates', 'ltcf_long_list.xlsx')
  cases = file.path(fold, 'cases.csv')
  beds <- file.path('templates', 'ltcf_licensed_comprehensive.csv')

  acrciq <- file.path(fold, 'acrciq.xlsx')
  if(!file.exists(acrciq)) acrciq <- file.path(fold, 'acrciq.csv')
  chgs <- file.path(fold, 'chgs.xlsx')
  if(!file.exists(chgs)) chgs <- file.path(fold, 'chgs.csv')

  donotallocate <- file.path(fold, 'donotallocate.xlsx')
  if(!file.exists(donotallocate)) donotallocate <- file.path(fold, 'donotallocate.csv')

  #Outputs
  fillable = file.path(output, paste0('asum_fillable', suffix,'.csv'))
  considered = file.path(output, paste0('asum_consider', suffix,'.csv'))
  allords = file.path(output, paste0('asum_all', suffix,'.csv'))
  lefts = file.path(output, paste0('leftovers', suffix,'.csv'))
  lefts_sum = file.path(output, paste0('leftovers_sum', suffix,'.csv'))
  oot_excel = file.path(output, paste0('picklist', suffix,'.xlsx'))
  out_excel_by_tier = file.path(output, paste0('picklist', suffix,'_tier_', runtiers, '.xlsx'))
  out_excel_by_region = file.path(output, paste0('picklist', suffix,'_region_', regions, '.xlsx'))
  oot_wide = file.path(output, paste0('picklist_wide', suffix,'.csv'))
  oot_dr = file.path(output, paste0('distribution_report', suffix,'.csv'))
  oot_no_1 = file.path(output, paste0('no_allocation_wanum', suffix,'.csv'))
  oot_no_2 = file.path(output, paste0('no_allocation_orders', suffix,'.csv'))
  oot_gowns = file.path(output, paste0('unfilled_gowns', suffix,'.csv'))
  oot_ltcf_1 = file.path(output, paste0('ltcf_allocations', suffix,'.csv'))
  oot_ltcf_2 = file.path(output, paste0('ltcf_allocations_sum', suffix,'.csv'))
  oot_ltcf_3 = file.path(output, paste0('ltcf_allocations_factype', suffix,'.csv'))
  oot_hosp_1 = file.path(output, paste0('hosp_allocations', suffix,'.csv'))
  oot_hosp_2 = file.path(output, paste0('hosp_allocations_sum', suffix,'.csv'))
  sum_cycle = file.path(output, paste0('sum_cycle_valid', suffix,'.csv'))
  mismatch_out = file.path(output, paste0('mismatch', suffix,'.csv'))
  oot_weights = file.path(output, paste0('weights', suffix,'.csv'))

  #The plan
  plan <- drake_plan(

    #load ppe and do initial tiering
    ppe = target(load_ppe_requests(file_in(!!orders), file_in(!!item_class), file_in(!!tiering), !!sized_items)),

    #addresses
    ads = target(load_ppe_requests(file_in(!!orders), file_in(!!item_class), file_in(!!tiering), !!sized_items, TRUE)),

    #load inventory
    inv = target(load_inventory(file_in(!!inv_fp), !!sized_items, !!holdback_frac)),

    #load and format hospital data
    hospital = target(load_hospital_data(file_in(!!hosp), !!hosp_supply)),

    #load and format ltcf data
    ltcf = target(load_ltcf_data(ppe, !!ltcf_categories, file_in(!!residents), file_in(!!beds), file_in(!!cases))),

    #create weights
    wt = target(create_weights(ppe, hospital, ltcf, file_in(!!acrciq), file_in(!!chgs))),

    #determine what orders to fill
    #also adjusts ltcfs into tier 1 and tier 1.5
    orders = target(order_filler(ppe, inv, ltcf, hospital, !!runtiers, ignore_items = !!ignore_me, inv_mismatch = FALSE, n95except = !!n95except)),

    #get where requests and inventory don't match
    mismatch = target(write.csv(order_filler(ppe, inv, ltcf, hospital, !!runtiers, ignore_items = !!ignore_me, inv_mismatch =  TRUE),
                                row.names = FALSE, file_out(!!mismatch_out))),

    #allocate and assign
    allocations = target(assign_and_allocate(orders, inv, wt,ltcf_categories = !!ltcf_categories, file_in(!!replacement_file), file_in(!!donotallocate))),

    #confirm allocations don't overallocate and create leftovers summary
    leftovers = target(find_leftovers(inv, allocations)),

    write_left = target(write.csv(leftovers, row.names = F,
                                  file_out(!!lefts))),
    left_sum = target(write.csv(summarize_leftovers(leftovers), row.names = F, file_out(!!lefts_sum))),
    #construct full agency summary
    sum_full = target(agency_summary(allocations, type = 'all',
                                     outpath = file_out(!!allords),
                                     tiers = !!runtiers)),

    #construct summary by type and item category

    #create a pick list and write (wide format) and add delivery info
    pl_wide = target(create_wide_pl(allocations, ads, !!standardize_chinook_addresses)),

    #write to excel
    out_excel = target(save_picklist(pl_wide, !!template, file_out(!!oot_excel))),

    # by tier
    out_xl_tier = target(save_picklist(pl_wide, !!template, file_out(a), t), transform = map(t = !!runtiers, a = !!out_excel_by_tier, .id = t)),

    #write out wide picklist
    out_wide = target(write.csv(pl_wide, file_out(!!oot_wide), row.names = F)),

    #by regional - TODO: add region to wide picklist from the tier sheet (currently generating as separate tab step)
    out_xl_region = target(save_region_picklist(pl_wide, !!template, file_out(a), r), transform = map(r = !!regions, a = !!out_excel_by_region, .id = r)),

    #write out weights
    out_weights = target(write.csv(wt, file_out(!!oot_weights), row.names = F)),

    #write out distribution report
    out_dr = target(write.csv(distribution_report(allocations, inv), row.names = F, file = !!oot_dr)),

    #write out no orders
    no_orders = target(no_order(sum_full, file_out(!!oot_no_1), file_out(!!oot_no_2))),

    #unfilled gowns
    unfil_gowns = target(write.csv(sum_full[fill_me == 1 & percent_filled<100 & item_type == 'gowns'], row.names = F, file = file_out(!!oot_gowns))),

    ltcf_sum = target(make_ltcf_summary(sum_full,file_in(!!residents), file_in(!!beds), file_out(!!oot_ltcf_1), file_out(!!oot_ltcf_2),file_out(!!oot_ltcf_3))),

    hosp_sum = target(make_type_summary(sum_full,'hospital', file_out(!!oot_hosp_1), file_out(!!oot_hosp_2))),

    overall = target(write.csv(summarize_cycle(sum_full),file = !!sum_cycle, row.names = F ))

  )

  make(plan, cache = cache)

  # files used in next cycle
  next_cycle_fp = file.path(fold, 'next_cycle_files')
  template_dropshipment <- file.path('templates', 'template_dropshipment.xlsx')
  template_orderadditions <- file.path('templates', 'order_additions.xlsx')
  next_cycle_files <- list(inv_fp,allords,acrciq,chgs,donotallocate,replacement_file,template_dropshipment,template_orderadditions)

  # delete prior directory/files
  if (dir.exists(next_cycle_fp)){
    unlink(next_cycle_fp,recursive = T, force = T)
  }

  # create directory and copy latest files
  dir.create(next_cycle_fp)
  copy_files = lapply(next_cycle_files, function(x) file.copy(x, next_cycle_fp))

  print('Allocations Complete!')
}
