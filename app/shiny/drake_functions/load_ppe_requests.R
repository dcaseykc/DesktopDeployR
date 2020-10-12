#' Load and standardize the ppe requests
#' @param orders file path that make up the PPE orders
#' @param item_class file path that converts PPE item requested into standardized types
#' @param agency_class file path to the document that provides information about tiering, agency type and the like
#' @param sized_items character. Vector of item types that are going to be distributed by size
#' @param ad_only logical. Return only the addresses

# orders = "C:/Users/dcasey/ppe/distribute_4-24_order/order_list_424_v2.xlsx"
# item_class = "C:/Users/dcasey/ppe/distribute_4-24_order/item_classifications2.csv"
# tiering = "C:/Users/dcasey/ppe/distribute_4-24_order/tiers_424_adj_v2.xlsx"
# sized_items = c('non-latex gloves', 'scrub pants', 'scrub pants', 'latex gloves', 'N95', 'coveralls')

load_ppe_requests = function(orders, item_class, tiering, sized_items, ad_only = FALSE){

  ppe = (load_spreadsheet(orders))

  if('POC' %in% names(ppe)) setnames(ppe, 'POC', 'poc')
  items = load_spreadsheet(item_class)
  tiers = load_spreadsheet(tiering)

  ppe[, wa_num := trimws(wa_num, whitespace = "[\\h\\v]")]
  tiers[, wa_num := trimws(wa_num, whitespace = "[\\h\\v]")]
  
  ppe[, agency := trimws(agency, whitespace = "[\\h\\v]")]
  tiers[, agency := trimws(agency, whitespace = "[\\h\\v]")]
  
  ppe[, item_requested := trimws(item_requested)]
  items[, item := trimws(item)]
  
  setnames(items, 'item', 'item_requested')
  ppe = ppe[!is.na(item_requested)]
  ppe[, item_requested := trimws(item_requested, whitespace = "[\\h\\v]")]

  ppe = ppe[, requested := as.numeric(requested)]
  ppe = ppe[!is.na(requested),]
  
  items = unique(items)
  
  item_maps = items[, .N, item_requested]
  if(any(item_maps[, N>1])){
    bad_items = unique(items[item_requested %in% item_maps[N>1, item_requested], og])
    stop(paste('The following items feature duplicate classifications', paste0(bad_items, collapse = '; ')))
  }

  ppe = merge(ppe, unique(items[!is.na(item_requested)]), all.x = T, by = 'item_requested')
  
  #adjust hand sanitizer requests
  #assumes all inventory is in gallons which is not strictly true
  ppe[item_type == 'sanitizer, hand- bulk', requested := ceiling(requested * as.numeric(size))]
  ppe[item_type %in% c('sanitizer, hand- bulk', 'sanitizer, hand- bottle'), size := NA]
  mis = unique(ppe[!is.na(item_requested) & is.na(item_type), item_requested])
  if(length(mis) >0){
    stop(paste('Missing item class for: ', paste0(mis, collapse = ', ')))
  }
  
  stopifnot(all(!is.na(ppe[!is.na(item_requested), item_type])))
  
  #replace address in ppe if its coming through tiers
  if('address' %in% names(tiers) && 'address' %in% names(ppe)){
    ppe[, address := NULL]
  }
  if('lnum' %in% names(ppe) && 'lnum' %in% names(lnum)){
    ppe[, lnum := NULL]
  }
  
  #confirm one agency per wa num
  stopifnot(all(unique(ppe[, .(wa_num, agency)])[, .N, .(wa_num, agency)][, N] %in% 1))
  stopifnot(all(unique(tiers[, .(wa_num, agency)])[, .N, .(wa_num, agency)][, N] %in% 1))
  
  #make the tiers unique
  #tiers[, agency := NULL]
  tiers[!is.na(newname) & newname != "", agency := newname]

  tiers = unique(tiers[, .(wa_num, agency, address, lnum, type, current.tier, priority, region)])
  ppe[, agency := NULL] #changing this up to fix excel drag errors in a lazy way
  
  stopifnot('Missing classifications in tier sheet' = all(!is.na(tiers[, agency]) & !tiers[, agency] %in% ''))
  stopifnot('Missing tiering in tier sheet' = all(!is.na(tiers[, current.tier]) & !tiers[, current.tier] %in% ''))
  stopifnot('Missing region in tier sheet' = all(!is.na(tiers[, region]) & !tiers[, region] %in% ''))
  
  tiers_by_wa = tiers[, .N, .(wa_num)]
  
  if(nrow(tiers_by_wa[N>1])>0){
    bad_was = tiers_by_wa[N>1, wa_num]
    stop(paste0('In the tier sheet, the following WA nums are not unique by at least one of agency, address, lnum, type, current.tier, or priority: '),
         paste0(bad_was, collapse = ', '))
  }
  
  ppe_st = nrow(ppe)
  ppe = merge(ppe, tiers, all.x = T, by = c('wa_num'))

  stopifnot('Likely duplicate agencies per wa num in tiers' = (ppe_st) == nrow(ppe))
  
  ppe[, type := tolower(type)]
  
  stopifnot(all(unique(ppe[, .(agency, wa_num)])[, .N, .(agency, wa_num)][, N==1]))
  
  ppe[, order_ids := paste(unique(wa_num), collapse = ', '), 
      by = .(type, current.tier, agency)]

  ads = unique(ppe[, .(wa_num, agency, poc, phone, address, email, region)])
  if(ad_only){
    return(ads)
  }
  
  ppe[!item_type %in% sized_items, size := 'any size']
  ppe = ppe[, .(order_ids = unique(order_ids),
                requested = sum(requested)),
            by = .(item_type, size, type, tier = current.tier, agency, priority, lnum)]
  ### make classifications to inventory ppe
  ## create an itemz category that optionally takes into account size or other modifiers
  ppe[, itemz := paste0(item_type,', ', size)]
  
  ppe[lnum == '9999999', lnum := NA]
  
  ppe[type %in% c('alternate care facility', 'iq', 'acrc'), type := 'acrc/iq']
  
  ppe[, ppe_id := .I]
  
  ppe[is.na(priority), priority := 99]
  
  #check for stupid license duplicates
  ppe = fix_ltcf_lnums(ppe, 'lnum', 'agency')
  
  #remove requests for 0 or less items
  ppe = ppe[requested>0]
  return(ppe)
  
}
