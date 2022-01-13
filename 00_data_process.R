
library(data.table)
library(openxlsx)
library(Hmisc)
library(survey)



in.path = "~/Documents/lgbtq_project/Data/" 
out.path = "~/Documents/lgbtq_project/Data/" 

## Load public datasets - only needed to be done once
in.path = "~/Documents/data_counties/Data/Raw/"
# states_a_m = fread(paste0(in.path,"table_a_m.csv"))
# states_n_z = fread(paste0(in.path,"table_n_z.csv"))
# states_combined = rbind(states_a_m, states_n_z)
# fwrite(states_combined, paste0(out.path,"SADCQ_2019_State.csv"))

states_combined_dt = fread(paste0(out.path,"SADCQ_2019_State.csv"))
states_combined_dt[sitecode == "AZB", sitecode := "AZ"]

state_svy_qn <- svydesign(id = ~PSU, weight = ~weight, strata = ~stratum,
                               data = states_combined_dt, nest = TRUE)

#old = fread("~/Documents/lgbtq_project/From Johannes/SADCQ_2017_State.csv")

#Necessary ID variables:
id_vars = c(
  'sitecode', 'year', 'weight', 'stratum', 'psu'
)

varimp_inds = as.data.table(read.xlsx(paste0('~/Documents/lgbtq_project/Data/colname_mapping_2019.xlsx')))
modeling_vars = varimp_inds[, .(var, yrbs_2017)] #varimp_inds[incl_v2 == "y", var]
missing_2019 <- modeling_vars[is.na(var)] 
modeling_vars = modeling_vars[ ,var]
modeling_vars = modeling_vars[!is.na(modeling_vars)]##Note NA col due to q40

#Public dataset subset
ss_vars = c(id_vars,modeling_vars)
public_ss_vars = union(ss_vars, c('race4'))
public_ss_vars = tolower(public_ss_vars)
public_ss_vars = public_ss_vars[!is.na(public_ss_vars)]
colnames(states_combined_dt) = tolower(colnames(states_combined_dt))
public_state_subset = states_combined_dt[, ..public_ss_vars]
public_subset = states_combined_dt[, ..public_ss_vars]

 # Load individual state data
in.path =  "~/Documents/lgbtq_project/Data/individual_states/" 
stateFiles = paste0(in.path,grep("", list.files(in.path), value = T))
stateFiles = c(stateFiles[grepl("MA",stateFiles)], 
               stateFiles[grepl("OH",stateFiles)],
               stateFiles[grepl("IN",stateFiles)])
state_DTs = sapply(stateFiles, fread, USE.NAMES = T, simplify = F)
for (dt in state_DTs) {
  setnames(dt, tolower(colnames(dt)))
}
names(state_DTs) <- gsub(in.path,"", names(state_DTs))


#Map variables to public column names
map_variables_to_public_colnames = function(in_dt, sitecode, year, mapping_dt, in_col_names_in_mapping_dt) {
  
  out_dt = copy(in_dt)
  for(i in seq(1, nrow(mapping_dt))) {
    out_colname = mapping_dt$var[i]
    in_colname = mapping_dt[, get(in_col_names_in_mapping_dt)][i]
    if(is.na(in_colname)) {
      out_dt[, (out_colname) := as.numeric(NA)]  
    } else {
      out_dt[, (out_colname) := in_dt[, as.numeric(get(in_colname))]] 
    }
  }
  
  out_dt[, year := year]
  out_dt[, sitecode := sitecode]
  out_dt = out_dt[, ..ss_vars]
  out_dt[, public := "N"]
  return(out_dt)
}

# MA 2019
ma_2019_out = map_variables_to_public_colnames(
  in_dt = state_DTs$MA_2019_q.csv,
  sitecode = "MA",
  year = 2019,
  mapping_dt = varimp_inds[, .(var, MA_2019)],
  in_col_names_in_mapping_dt = "MA_2019"
)

# MA 2017
ma_2017_out = map_variables_to_public_colnames(
  in_dt = state_DTs$MA_2017_q.csv,
  sitecode = "MA",
  year = 2017,
  mapping_dt = varimp_inds[, .(var, MA_2017)],
  in_col_names_in_mapping_dt = "MA_2017"
)

# MA 2015
ma_2015_out = map_variables_to_public_colnames(
  in_dt = state_DTs$MA_2015_q.csv,
  sitecode = "MA",
  year = 2015,
  mapping_dt = varimp_inds[, .(var, MA_2015)],
  in_col_names_in_mapping_dt = "MA_2015"
)

# OH 2013
oh_2013_out = map_variables_to_public_colnames(
  in_dt = state_DTs$OH_2013_q.csv,
  sitecode = "OH",
  year = 2013,
  mapping_dt = varimp_inds[, .(var, OH_2013)],
  in_col_names_in_mapping_dt = "OH_2013"
)

# OH 2019
oh_2019_out = map_variables_to_public_colnames(
  in_dt = state_DTs$OH_2019_q.csv,
  sitecode = "OH",
  year = 2019,
  mapping_dt = varimp_inds[, .(var, OH_2019)],
  in_col_names_in_mapping_dt = "OH_2019"
)

# IN 2015
in_2015_out = map_variables_to_public_colnames(
  in_dt = state_DTs$IN_2015_q.csv,
  sitecode = "IN",
  year = 2015,
  mapping_dt = varimp_inds[, .(var, IN_2015)],
  in_col_names_in_mapping_dt = "IN_2015"
)


# Combined data
public_w_states = rbindlist(list(
  public_subset[, public := "Y"], 
  in_2015_out,
  oh_2013_out,
  oh_2019_out,
  ma_2015_out,
  ma_2017_out,
  ma_2019_out
  ), fill=T)


#Identify missing columns - double checked these are truly missing
all_cols = colnames(public_w_states)

for(c in all_cols) {
  subset_dt = public_w_states[public == "N"]
  subset_dt = subset_dt[, .(non_missing = sum(!is.na(get(c)))), by = c('sitecode', 'year')]
  if(nrow(subset_dt[non_missing==0])>0){
    print(paste("Column:", c, ", missing in:"))
    print(subset_dt[non_missing==0, paste(sitecode,year)])
  }
}


#Look at differences in max/min modeling_vars from public dataset when not missing
# Get rid of weird values identified below
public_w_states[grade == 5, grade := NA]

for(c in modeling_vars) {
  
  # Check that have non-missing values in non-public; otherwise next
  if(public_w_states[public == "N" & !is.na(get(c)), .N] == 0) {
    next
  }
  
  public_min = public_w_states[public == "Y" & !is.na(get(c)), min(get(c))]
  public_max = public_w_states[public == "Y" & !is.na(get(c)), max(get(c))]
  
  # non-public min and maxes by state-year
  nonpublicsaggs = public_w_states[public == "N" & !is.na(get(c)), .(
    min = min(get(c)),
    max = max(get(c))
    ), by = c('sitecode', 'year')]
  
  # subset to values outside public data
  nonpublicsaggs = nonpublicsaggs[min < public_min | max > public_max]
  
  if(nrow(nonpublicsaggs)>0) {
    print(paste("Column:", c, ", weird in:"))
    print(nonpublicsaggs[, paste(sitecode,year)])
    nonpublicsaggs[, `:=`(public_min = public_min, public_max = public_max, col = c)]
    print(nonpublicsaggs)
  }
}

#Write out combined data
site_regions_divisions = fread("~/Documents/lgbtq_project/Data/state_regions_divisions.csv")
public_w_states = merge(public_w_states, site_regions_divisions, by='sitecode')

# filter to state - years with answers to q66 OR q67 (2019 version is 66 or 65)
public_w_states[, include := sum(!is.na(q66) | !is.na(q65)), by=c('sitecode','year')]
public_w_states[, include := sign(include)]
public_w_states[, table(include)]

# test compared to 2017 dataset
test = fread("~/Documents/lgbtq_project/From Johannes/combined_pred_data_all.csv")
test2 = test[include == 1]

for(mm in unique(test$sitecode)){
  print(mm)
  al1 = test[sitecode == mm]
  al2 = public_w_states[sitecode == mm]
  print(all.equal(table(al1$year),table(al2$year[al2$year != 2019])))
}

#CT, GA, MD, NM, TX, VT have extra historical years creating mismatch from 2017 data.
fwrite(public_w_states[include==1], paste0(out.path,'combined_pred_data.csv'))
fwrite(public_w_states, paste0(out.path,'combined_pred_data_all.csv'))



