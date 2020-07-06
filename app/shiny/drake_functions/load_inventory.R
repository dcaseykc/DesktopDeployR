#' Load the inventory.
#' @param fp file path to inventory file.
#' @param sized_items character. Vector of item types where we care about sizing.
#' 
#inv_fp <- ("C:/Users/dcasey/OneDrive - King County/covid/PPE disbursement/distribute_4-17_order/inv/Inventory_Balance_28Apr2020_1200AM_adj.csv")
load_inventory <- function(inv_fp, sized_items, prop_allocate = .95){
  
  inv = load_spreadsheet(inv_fp)
  
  inv = inv[ship_u>0,]
  inv[, Source := Item]
  inv[, Item := tolower(Description)]
  inv = inv[, .(Item, Source, ship_u, each_per_su, item_type, size)]
  stopifnot(all(inv[,.N, Source][, N==1]))
  
  ## make sure each row has a unique id
  inv[, Item_long := paste0(Source, ': ', Item)]
  inv[, id := Source]
  inv[, item_id := id]
  inv[size == "", size := NA]
  inv[!item_type %in% sized_items, size := 'any size']
  #inv[, itemz := item_type]
  inv[, itemz := paste0(item_type,', ', size)]
  
  #catch for missing items
  mis_eachper = inv[is.na(each_per_su), Source]
  mis_su = inv[is.na(ship_u), Source]
  mis_item_type = inv[is.na(item_type), Source]
  
  m = unique(c(mis_eachper, mis_su, mis_item_type))
  
  if(length(m) > 0){
    stop(paste('Missing info in items:', paste0(m, collapse = ', ')))
  }
  
  inv[, ship_u := ceiling(ship_u * prop_allocate)]
  
  setorder(inv, itemz, each_per_su, ship_u)
  
  return(inv)
  
}
