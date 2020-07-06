# #drake::r_make('C:/Users/dcasey/Documents/code/ppe/distribute_v4.R')
# library('data.table')
# library('openxlsx')
# library('drake')
# library('tools')
# set.seed(1)
# runtiers = c(1,2,3, 1.5)
# suffix = '_0501_v2'
# cloc = 'C:/Users/dcasey/Documents/ppe_runs/distribute_05-01_order/'
# cloc = file.path(cloc, suffix)
# base = 'C:/Users/dcasey/ppe/distribute_05-01_order/'
# output = file.path(base, suffix)
# 
# tiering = "C:/Users/dcasey/ppe/distribute_05-01_order/tiers_0501.xlsx"
# orders = "C:/Users/dcasey/ppe/distribute_05-01_order/order_list_0501.xlsx"
# allords = file.path(output, paste0('asum_all', suffix,'.csv'))
# 
# for (fff in list.files('C:/Users/dcasey/Documents/code/ppe/drake_functions/', '\\.R$', full.names = T)) {
#   source(fff)
# }

#' Identify which orders got nothing
#' 
#' @param asum asum all
#' @param ns1 file path. Output of the wa nums (and other contact info)
#' @param ns2 file path. Output of the order list
no_order = function(asum, ns1, ns2){

  #Where are total allocations 0
  asum[, total_alloc := sum(allocated), by = .(order_ids, agency, type)]
  nadda = asum[total_alloc == 0, ]
  
  naddalist = trimws(unique(unlist(strsplit(nadda[,order_ids], ',', fixed = T))))
  
  write.csv(data.table(no_items = naddalist), file = ns1, row.names = F)
  write.csv(nadda, file = ns2, row.names = F)
  
  return(list(data.table(no_items = naddalist),nadda))
  
}