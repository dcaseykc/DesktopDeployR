#' Prepare a new set of order and tiers sheets
#' @param fold file.path- path to the directory of the working folder (e.g. where outputs get saved)
#' @param date date- the date (usually derived from \code{\link{Sys.Date()}}) representing the closing day of the cycle. Usually this is the Friday the week before.
#' @param t1 file.path- path to the excel/csv file containing the tier 1 requests. This comes from Logs.
#' @param t2 file.path- path to the excel/csv file containing the tier 2+ requests. This comes from Logs
#' @param order_v numeric/character- new version name
#' @param load_from_previous logical. Determines whether previous tiering information (from the same date cycle) should be carried forward to a new version of the tiers and orders
#' @param prev_v numeric/character- old version name
#' @param previous_week file.path- Path to the location of the previous week's "asum_all" file for comparing old vs. new tracking numbers to ensure no duplication
#' @param dump file.path- Path to the location of a data dump from webeoc. This sheet brings the contact information.
#' @param add_fp file.path- Optional. Path to the location of an excel/csv sheet specifying additional orders
#' @details This function creates blank/new versions of the order and tiers files. Unless provided a new order_v, this function will overwrite existing work.
#' @return A character string of the file path to the saved tiers file.
order_and_tiers = function(fold, date, t1, t2, order_v, load_from_previous, prev_v,
                           previous_week, dump, add_fp){

  notes = list('None', 'None','None')

  #Prepare date/cycle information
  cycle_mo = month(date)
  cycle_day = mday(date)

  #Read in the orders
  t1 = load_spreadsheet(t1)
  t2 = load_spreadsheet(t2)

  # dump = load_spreadsheet(dump)
  dump = read_excel(dump,sheet = 2)
  dump = setDT(dump)

  previous_week = load_spreadsheet(previous_week)
  if(!missing(add_fp) && !is.null(add_fp)) add = load_spreadsheet(add_fp)

  #delivery routes by zip
  routes = load_spreadsheet(file.path('./templates/routes.xlsx'))
  
  #default tier and type
  tier_type_defaults = load_spreadsheet(file.path('./templates/order_tier_defaults.xlsx'))

  stopifnot(all(names(t2) %in% names(t1)))

  orders = rbind(t1, t2, fill = T)

  if(!nrow(orders) == nrow(unique(orders))){
    warning('Duplicate entries in the order sheets.')
    notes[[1]] <- 'Duplicate entries in the order sheets. This is usually fine/represents duplicate rows in the underlying request files.'
  }

  if(exists('add') && inherits(add, 'data.table')){
    stopifnot(all(names(add) %in% names(orders)))
    orders = rbind(orders, add, fill = TRUE)
  }

  #standardize orders
  setnames(orders, c("King.County.Tracking.Number", "Agency.Name", "Facility.Type",
                     "License./.ID", "Item.Requested.(Paste.item.requested.from.thecity,.tribal.resource.request.form.here)",
                     "Enter.the.total.quantity.of.items.(Each).needed.(Paste.quantity.requested.here)",
                     "Tier.Number", "POC", "POC.Email", "POCPhone", "Delivery.Address", "Requestor.Email"),
           c('wa_num', 'agency', 'logs_type', '_lnum', 'item_requested', 'requested', 'logs_tier', 'poc', 'email', 'phone', 'address', 'email2')
  )

  orders = orders[, .(wa_num, agency, logs_type, `_lnum`, item_requested, requested, logs_tier)]
  orders[, wa_num := trimws(wa_num, whitespace = "[\\h\\v]")]

  #fix addresses in the orders
  dump = dump[county == 'King']
  dump[, wa_num := trimws(rndNum, whitespace = "[\\h\\v]")]
  dump = dump[wa_num %in% orders[, wa_num]]
  dump = dump[, .(wa_num, address, email = ReqSiteEmail, phone = ReqSitePhone, POC = ReqSitePOC, status = ReqStatus)]
  dump[, address := gsub('\t', "", address, fixed = T)]
  dump = unique(dump)

  # extract zip
  # should clean up address more (trailing spaces)
  dump[, zip:= str_extract(address, "(?<= )\\d{4,5}(?=\\n|$)")]
  dump = merge(dump, routes, all.x = T, by = 'zip')

  if(!all(unique(orders[, wa_num]) %in% dump[, wa_num])){
    misnums = setdiff(unique(orders[, wa_num]), dump[,wa_num])
    stop(paste0('The following wa nums are missing from the data dump: ', paste(misnums, collapse = ', ')))
  }

  dump = dump[wa_num %in% orders[, wa_num]]

  #For reasons I can't fathom, webeoc allows multiple discrete requests to have the same tracking number.
  #The next several lines check for that and use a few heuristics to determine whether that is a problem
  multi_dump = dump[, .N, wa_num][N>1]

  if(nrow(multi_dump)>0){

    dump = dump[!(wa_num %in% multi_dump[, wa_num] & !status %in% "Order in Process")]

    md_2 = dump[, .N, wa_num][N>1]
    if(nrow(md_2) > 0){
      stop(paste0('Error: tracking numbers (wa_num) are not unique per request: ', paste0(md_2[, wa_num], collapse = ', '),'.',
                  'This usually means WebEOC has assigned the same id to two different requests. Best way to fix this is to get logs to create a new entry and then replace the underlying data and rerun.'))
    }else{
      msg = paste0('WA nums were not unique to a request, but filtering on status made them unique. Worth double checking: ', paste0(multi_dump[, wa_num], collapse = ', ') )
      warning(msg)
      notes[[2]] <- msg
    }
  }

  start_n = nrow(orders)
  orders[, n_r_start := .N, wa_num]
  orders = merge(orders, dump, all.x = T, by = 'wa_num')
  orders[, n_r_end := .N, wa_num]
  end_n = nrow(orders)

  if(end_n != start_n){
    stop(end_n - start_n)
  }

  #remove stupid characters from item names
  orders[, item_requested := gsub('"', "", item_requested, fixed = T)]

  #force numerics
  orders[, requested := as.numeric(requested)]

  #create tiering file
  tiers = orders[, .(wa_num, agency, address, zip, region, lnum = `_lnum`, newname = "",
                     notes = "", priority = "", logs_type, logs_tier)]
  tiers = unique(tiers)

  if(start_n != end_n){
    stop('WEBEOC PROBABLY FARTED DUPLICATE IDs AGAIN')
  }else{

    ttt = unique(tiers[ ,.(wa_num, agency, address, zip, region, lnum = lnum, newname = "",
                           notes = "", priority = "", logs_lnum = lnum, logs_type, logs_tier)])

    ttt[,lnum := gsub('/', ', ', lnum, fixed = T)]
    #check previous week
    prev_ids = trimws(unlist(strsplit(previous_week[, order_ids], split = ',', fixed = T)), whitespace = '[\\h\\v]')
    dupes = intersect(ttt[, wa_num], prev_ids)

    #join default tiers and types
    ttt = merge(ttt, tier_type_defaults, all.x = T, by="logs_type")
    
    if(load_from_previous){
      old = load_spreadsheet(file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', prev_v, '.xlsx')))
      
      ttt = rbind(old, ttt[!wa_num %in% old[, wa_num]])
    }

    # don't need region in Order List as they are reviewed/updated in Tiers
    orders = select(orders, -c('region'))
    
    write.xlsx(orders, file.path(fold, paste0('order_list_', cycle_mo, cycle_day,'_', order_v, '.xlsx')))
    
    # reset field order
    ttt = select(ttt, c('wa_num','agency','address','zip','region','lnum','type','newname','notes','current.tier','priority','logs_lnum','logs_tier','logs_type'))
    
    write.xlsx(ttt, file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', order_v, '.xlsx')))
    write.csv(ttt, file.path(fold, paste0('tiers_raw.csv')), row.names = F, na = "")

    if(length(dupes)>0){
      warning(paste('IDs also from last week:', paste0(dupes, collapse = ', ')))
      notes[[3]] <- paste('IDs also from last week:', paste0(dupes, collapse = ', '))
    }

  }

  return(paste0('Tier File: ', file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', order_v, '.xlsx')),
                ' Warning 1: ', notes[[1]], ' Warning 2: ', notes[[2]], ' Warning 3: ', notes[[3]]))


}
