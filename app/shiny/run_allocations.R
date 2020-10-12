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
  ltcf = target(load_ltcf_data(ppe, !!ltcf_categories, file_in(!!residents), file_in(!!beds), file_in(!!cases), file_in(!!cw))),

  #create weights
  wt = target(create_weights(ppe, hospital, ltcf, file_in(!!acrciq), file_in(!!chgs))),

  #determine what orders to fill
  #also adjusts ltcfs into tier 1 and tier 1.5
  orders = target(order_filler(ppe, inv, ltcf, hospital, !!runtiers, ignore_items = !!ignore_me, inv_mismatch = FALSE)),

  #get where requests and inventory don't match
  mismatch = target(order_filler(ppe, inv, ltcf, hospital, !!runtiers, ignore_items = !!ignore_me, inv_mismatch =  TRUE)),

  #allocate and assign
  allocations = target(assign_and_allocate(orders, inv, wt,ltcf_categories = !!ltcf_categories, !!replacement_file)),

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

  out_xl_tier = target(save_picklist(pl_wide, !!template, file_out(a), t), transform = map(t = !!runtiers, a = !!out_excel_by_tier, .id = t)),

  #write out wide picklist
  out_wide = target(write.csv(pl_wide, file_out(!!oot_wide), row.names = F)),

  #write out distribution report
  out_dr = target(write.csv(distribution_report(allocations, inv), row.names = F, file = !!oot_dr)),

  #write out no orders
  no_orders = target(no_order(sum_full, file_out(!!oot_no_1), file_out(!!oot_no_2))),

  #unfilled gowns
  unfil_gowns = target(write.csv(sum_full[fill_me == 1 & percent_filled<100 & item_type == 'gowns'], row.names = F, file = file_out(!!oot_gowns))),

  ltcf_sum = target(make_ltcf_summary(sum_full,file_in(!!residents), file_out(!!oot_ltcf_1), file_out(!!oot_ltcf_2),file_out(!!oot_ltcf_3))),
  
  hosp_sum = target(make_type_summary(sum_full,'hospital', file_out(!!oot_hosp_1), file_out(!!oot_hosp_2))),
  
  overall = target(write.csv(summarize_cycle(sum_full),file = !!sum_cycle, row.names = F ))
  
)

a = drake_config(plan, cache_log_file = file.path(output,suffix,'log.csv'),
                 cache = cache)
#a
