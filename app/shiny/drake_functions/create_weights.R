# ppe  = readd(ppe, cache = cache)
# hospital = readd(hospital, cache = cache)
# ltcf = readd(ltcf, cache = cache)


create_weights = function(ppe, hospital, ltcf, acrciq, chgs){

  acrciq = load_spreadsheet(acrciq)
  chgs = load_spreadsheet(chgs)

  w_acrc = acrciq[agency %in% ppe[, agency], .(agency, type = 'acrc/iq', chg = npeople * chgs[type == 'acrc/iq', chg], w2 = npeople, changes_formula='npeople * ppe_burn_rate', w2_def='people', ppe_burn_rate=chgs[type == 'acrc/iq', chg], case_count=-1)]
  w_ltcf = ltcf[agency %in% ppe[, agency], .(agency, type = 'ltcf', chg = cases * chgs[type == 'ltcf', chg], w2 = nbeds, changes_formula='cases * ppe_burn_rate', w2_def='beds', ppe_burn_rate=chgs[type == 'ltcf', chg], case_count=cases)]
  w_ltcf[is.na(chg), chg := 1] #place holder for facilities with no cases
  w_hosp = hospital$hosp_cases[name %in% ppe[, agency], .(agency = name, type = 'hospital', chg = covid * chgs[type == 'hospital', chg], w2 = covid, changes_formula='covid cases * ppe_burn_rate', w2_def='covid cases', ppe_burn_rate=chgs[type == 'hospital', chg], case_count=covid)]
  w_ph = unique(ppe[type %in% 'public health agency', .(agency, type, chg = chgs[type == 'public health agency', chg], w2 = 1, changes_formula='ppe_burn_rate', w2_def='1', ppe_burn_rate=chgs[type == 'public health agency', chg], case_count=-1)])

  if(length(unique(ppe[type == 'ems', agency]))>1) warning('More than one EMS request detected')
  w_ems = unique(ppe[type == 'ems', .(agency, type, chg = chgs[type == 'ems', chg], w2 = 1, changes_formula='Total number of times a responder noted PPE use', w2_def='1', ppe_burn_rate=chgs[type == 'ems', chg], case_count=-1)])

  #create a combined weight matrix
  w = rbind(w_acrc, w_ltcf, w_hosp, w_ems, w_ph)

  miss = setdiff(unique(ppe[type %in% w[,type], agency]), w[, agency])
  if(length(miss)>0){
    stop(paste0('Missing weights for: ', paste0(miss, collapse = ', ')))
  }

  #create the weights for ltcfs without cases
  w_sub = unique(ppe[!agency %in% w[,agency], .(agency, chg = 1, w2 = 1, changes_formula='1', w2_def='1', ppe_burn_rate=1, case_count=-1), type])

  w = rbind(w, w_sub)

  return(w)

}
