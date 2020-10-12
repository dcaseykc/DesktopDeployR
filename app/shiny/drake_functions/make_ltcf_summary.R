# asum = readd(sum_full, cache = cache)
# ltcflist = residents
make_ltcf_summary = function(asum, residents, beds, oot1, oot2, oot3){
  
  residents = fix_ltcf_lnums(load_spreadsheet(residents), 'License.number', 'Facility.Name')
  beds = fix_ltcf_lnums(load_spreadsheet(beds), 'LicenseNumber', 'FacilityName')
  bts = data.table(abbriv = c('AL', 'NF', 'AF'), type = c('Assisted Living Facility', 'Nursing Home', 'Adult Family Home'))
  #fix up the beds file
  residents = residents[Provider.County == 'King', .(lnum = License.number, res = as.numeric(`#.of.Residents`), type = `Provider.Type`, bedbackup = as.numeric(`Bed.Count`))][!is.na(type), ]
  beds = unique(beds[, .(lnum = LicenseNumber,abbriv = trimws(FacilityType), nbeds = LicensedBedCount)])
  beds = merge(beds, bts, all.x = T, by = 'abbriv')
  
  ltcf_type = merge(residents, beds[,.(lnum, nbeds, type)], by = c('lnum', 'type'), all = T)
  ltcf_type = unique(ltcf_type[, .(lnum, type)])

  asum = asum[type == 'ltcf']
  lnum_ids = asum[, tstrsplit(lnum, ',')]
  lnum_ids = lnum_ids[, lapply(.SD, function(x) trimws(x, whitespace = "[\\h\\v]"))]
  lnum_ids[, lnum := asum[, lnum]]
  
  stopifnot(all(!is.na(lnum_ids[, V1])))
  lnum_ids = unique(melt(lnum_ids, id.vars = 'lnum', value.factor = FALSE))
  lnum_ids = lnum_ids[!is.na(value)]
  lnum_ids = merge(lnum_ids, ltcf_type, by.x = 'value', by.y = 'lnum', all.x = T)
  
  stopifnot('License id likely not found in the long list-- check for types' = all(!is.na(lnum_ids[, type])))
  
  lnum_ids = unique(lnum_ids[, .(lnum, ltcf_type = type)])
  setorder(lnum_ids, ltcf_type)
  lnum_ids = lnum_ids[, .(ltcf_type = paste0(ltcf_type, collapse = ', ')), by = 'lnum']
  
  st = nrow(asum)
  asum = merge(asum, lnum_ids, all.x = T, by = 'lnum')
  stopifnot('Probably duplicate ids' = st == nrow(asum))
  
  asum[, type := ltcf_type]
  
  #collapse by item type
  itsum = asum[, .(requested = sum(requested), allocated = sum(allocated), type = 'All LTCFs'), by = item_type]
  itsum_type = asum[, .(requested = sum(requested), allocated = sum(allocated)), by = .(item_type, type)]
  itsum = rbind(itsum, itsum_type)
  itsum[, `percent filled` := round(allocated/requested * 100)]
  
  count_facility = unique(asum[, .(agency, type)])[, .N, type]
  
  write.csv(asum[, .(order_ids, agency, type, item_type, size, tier, fill_me, requested, allocated)], file = oot1, row.names = F)
  write.csv(itsum, file = oot2, row.names = F)
  write.csv(count_facility, file = oot3, row.names = F)
  return(list(asum, itsum))
}