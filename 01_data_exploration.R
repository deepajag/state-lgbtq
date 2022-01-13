

library(data.table)
library(openxlsx)
library(Hmisc)
library(survey)
library(rgdal)
library(rgeos)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(grid)




in.path = "~/Documents/lgbtq_project/Data/" 
out.path = "~/Documents/lgbtq_project/Data/Processed/" 

public_w_states = fread(paste0(in.path,'combined_pred_data_all.csv'))
public_w_states= fread("~/Documents/lgbtq_project/From Johannes/combined_pred_data_all.csv")
public_w_states[, .(max(year), uniqueN(year)), by='sitecode']

test_dt = copy(public_w_states)
q_cols = grep('^q', colnames(test_dt), value = T)
q_cols = c(q_cols, 'age','bmi','bmipct','grade')

test_dt[, non_null := Reduce(`+`, lapply(.SD,function(x) !is.na(x))), .SDcols = q_cols]
test_dt[, summary(non_null)]

#Data size
public_w_states[, have_q66 := max(ifelse(!is.na(q66), 1, 0)), by = .(sitecode, year)]
public_w_states[, have_q65 := max(ifelse(!is.na(q65) & !is.na(q59), 1, 0)), by = .(sitecode, year)]
public_w_states[, latest_year := max(year), by = 'sitecode']
public_w_states[, is_latest_year := max(year==latest_year)]

q_cols = grep('^q', colnames(public_w_states), value = T)
q_cols = c(q_cols, 'sex', 'age','bmi','bmipct','grade')

# Training data, LGB
print(public_w_states[year>= 2015 & have_q66==1, .N])
print(public_w_states[year>= 2015 & have_q66==1, uniqueN(sitecode)])
print(public_w_states[year>= 2015 & have_q66==1 & have_q65==1, uniqueN(sitecode)])

public_w_states[have_q66==1, Y_q66 := 0]
public_w_states[q66 == 2, Y_q66 := 1]
public_w_states[q66 == 3, Y_q66 := 1]

public_w_states[, non_null_cols := Reduce(`+`, lapply(.SD,function(x) !is.na(x))), .SDcols = q_cols]

per_state_metrics = public_w_states[year>= 2015 & have_q66==1,.(
  n_years = uniqueN(year),
  qs_with_answers = max(non_null_cols),
  true_prev = mean(Y_q66)
), by = 'sitecode']

print(per_state_metrics[, .(
  mean_n_years = mean(n_years),
  min_years = min(n_years),
  max_years = max(n_years),
  median_qs = quantile(qs_with_answers, 0.5),
  min_qs = min(qs_with_answers),
  max_qs = max(qs_with_answers),
  mean_prev = mean(true_prev),
  sd_prev = sd(true_prev)
)])

# Prediction data, LGB
print(public_w_states[year>= 2015 & is_latest_year & have_q66==0, .N])
print(public_w_states[year>= 2015 & is_latest_year & have_q66==0, uniqueN(sitecode)])
print(public_w_states[year>= 2015 & is_latest_year & have_q66==0 & have_q65==1, uniqueN(sitecode)])

per_state_metrics = public_w_states[year>= 2015 & is_latest_year & have_q66==0,.(
  n_years = uniqueN(year),
  qs_with_answers = max(non_null_cols),
  true_prev = mean(Y_q66)
), by = 'sitecode']

print(per_state_metrics[, .(
  mean_n_years = mean(n_years),
  min_years = min(n_years),
  max_years = max(n_years),
  median_qs = quantile(qs_with_answers, 0.5),
  min_qs = min(qs_with_answers),
  max_qs = max(qs_with_answers),
  mean_prev = mean(true_prev),
  sd_prev = sd(true_prev)
)])


# Training data, Sex of Contacts
print(public_w_states[year>= 2015 & have_q65==1, .N])
print(public_w_states[year>= 2015 & have_q65==1, uniqueN(sitecode)])
print(public_w_states[year>= 2015 & have_q65==1 & have_q66==1, uniqueN(sitecode)])

public_w_states[have_q65==1, Y_q65 := 0]
public_w_states[q65 == 4, Y_q65 := 1]
public_w_states[sex == 1 & q65 == 2, Y_q65 := 1]
public_w_states[sex == 2 & q65 == 3, Y_q65 := 1]

per_state_metrics = public_w_states[year>= 2015 & have_q65==1,.(
  n_years = uniqueN(year),
  qs_with_answers = max(non_null_cols),
  true_prev = mean(Y_q65)
), by = 'sitecode']

print(per_state_metrics[, .(
  mean_n_years = mean(n_years),
  min_years = min(n_years),
  max_years = max(n_years),
  median_qs = quantile(qs_with_answers, 0.5),
  min_qs = min(qs_with_answers),
  max_qs = max(qs_with_answers),
  mean_prev = mean(true_prev),
  sd_prev = sd(true_prev)
)])

# Prediction data, Sex of Contacts
print(public_w_states[year>= 2015 &is_latest_year & have_q65==0, .N])
print(public_w_states[year>= 2015 &is_latest_year & have_q65==0, uniqueN(sitecode)])
print(public_w_states[year>= 2015 &is_latest_year & have_q65==0 & have_q66==1, uniqueN(sitecode)])

per_state_metrics = public_w_states[year>= 2015 &is_latest_year & have_q65==0,.(
  n_years = uniqueN(year),
  qs_with_answers = max(non_null_cols),
  true_prev = mean(Y_q65)
), by = 'sitecode']

print(per_state_metrics[, .(
  mean_n_years = mean(n_years),
  min_years = min(n_years),
  max_years = max(n_years),
  median_qs = quantile(qs_with_answers, 0.5),
  min_qs = min(qs_with_answers),
  max_qs = max(qs_with_answers),
  mean_prev = mean(true_prev),
  sd_prev = sd(true_prev)
)])


#Write out combined trans data
state_years_w_trans = unique(public_w_states[!is.na(qtransgender), .(sitecode, year)])
state_dt_trans = merge(public_w_states, state_years_w_trans, by = c('sitecode','year'))
fwrite(state_dt_trans, '~/Downloads/YRBS/combined_pred_trans_data.csv')

#Only get ~20k more obs from districts. Can probably drop this question


### Maps of states and data availability
#https://rud.is/b/2015/05/15/u-s-drought-monitoring-with-hexbin-state-maps-in-r/

state_years_w_data = public_w_states[year>=2013, .(
  any_yrbs_data = 1,
  any_q65_data = max(ifelse(!is.na(q65),1,0)),
  any_q66_data = max(ifelse(!is.na(q66),1,0)),
  last_yrbs_year = max(year)
), by = .(sitecode)]

state_years_w_data[, data_availability := "YRBS without both focal Qs"]
state_years_w_data[last_yrbs_year == 2019, data_availability := "YRBS without both focal Qs"]
state_years_w_data[any_q65_data==1, 
                   data_availability := "YRBS without both focal Qs"]
state_years_w_data[any_q66_data==1, 
                   data_availability := "YRBS without both focal Qs"]
state_years_w_data[any_q65_data*any_q66_data==1, 
                   data_availability := "YRBS with both focal Qs"]

# Custom values
# state_years_w_data = rbind(
#   state_years_w_data[,.(sitecode, data_availability)],
#   data.table(sitecode = c("GA", "MA", "IN", "OH"), 
#              data_availability = rep("Pending",4)))


us <- readOGR("~/Downloads/YRBS/us_states_hexgrid.geojson")
centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
us_map <- as.data.table(fortify(us, region="iso3166_2"))

us_map_w_data_inds = merge(us_map, state_years_w_data, by.x='id', by.y='sitecode', all.x=TRUE)
us_map_w_data_inds[is.na(data_availability), data_availability := "Unavailable"]

ggplot() +
  geom_map(data=us_map[id!="DC"], map=us_map, aes(x=long, y=lat, map_id=id), color="white", size=0.5) +
  geom_map(data=us_map_w_data_inds[id!="DC"], map=us_map, aes(fill=data_availability, map_id=id)) +
  geom_text(data=centers, aes(label=id, x=x, y=y), color="white", size=4) + coord_map() + theme_minimal() +
  labs(x=NULL, y=NULL, title="YRBS Data Availability by State (2013-2017)") + 
  theme(plot.title=element_text(face="bold", hjust=0, size=14)) + 
  theme(panel.border=element_blank()) + 
  theme(panel.grid=element_blank()) + 
  theme(axis.ticks=element_blank()) +
  theme(strip.background=element_blank()) + 
  theme(axis.text=element_blank()) + 
  scale_fill_manual(values=c("#F44336", "gray", "#43A047", "black", "#00ACC1", "#AB47BC")) +
  labs(fill="Data Availability")



#State Prevalences by Q, year
same_sex_contact_by_state_year = public_w_states[
  q65 %in% c(2,3,4),
  .(prev = weighted.mean(
    ifelse(q65 == 4 | (sex == 1 & q65 == 2) | (sex == 2 & q65 == 3), 1, 0),
    weight, na.rm = T)), 
  by = c('sitecode','year')]

same_sex_contact_by_state_year

state_years = as.data.table(expand.grid(id = us_map[id!="DC",unique(id)], year = public_w_states[year >= 2013,unique(year)]))
state_years = merge(us_map, state_years, by='id', allow.cartesian=T)

us_map_w_data_q65 = merge(state_years, same_sex_contact_by_state_year, by.x=c('year','id'), by.y=c('year','sitecode'),
                          allow.cartesian=T, all.x=TRUE)

us_map_w_data_q65[,prev_missing := is.na(prev)]
center_q65 = merge(as.data.table(centers), unique(us_map_w_data_q65[,.(year,id,prev_missing)]),
                   by=c('id'))

ggplot() +
  geom_map(data=us_map_w_data_q65[id!="DC"], map=us_map, aes(x=long, y=lat, map_id=id), color="white", size=0.5) +
  geom_map(data=us_map_w_data_q65[id!="DC"], map=us_map, aes(fill=prev, color=!prev_missing, map_id=id)) +
  geom_text(data=center_q65, aes(label=id, x=x, y=y, color=!prev_missing), size=4) + coord_map() + theme_bw() +
  labs(x=NULL, y=NULL, title="Same Sex Contact (given any Contact)\nby State and Year") + 
  theme(plot.title=element_text(face="bold", hjust=0, size=20)) + 
  theme(panel.border=element_blank()) + 
  theme(panel.grid=element_blank()) + 
  theme(axis.ticks=element_blank()) +
  theme(strip.background=element_blank()) + 
  theme(axis.text=element_blank()) + 
  facet_wrap(~year, ncol = 1) +
  theme(strip.text=element_text(face="bold", hjust=0, size=18)) +
  scale_fill_gradient(na.value="white") +
  scale_colour_manual(values = c("white", "black"), breaks = c(TRUE, FALSE)) +
  guides(color=FALSE) +
  theme(legend.title = element_blank())

```


```{r, fig.height=10}
minority_identity_by_state_year = public_w_states[
  !is.na(q66),
  .(prev = weighted.mean(
    ifelse(q66 %in% c(2,3), 1, 0),
    weight, na.rm = T)), 
  by = c('sitecode','year')]

us_map_w_data_q66 = merge(state_years, minority_identity_by_state_year, by.x=c('year','id'), by.y=c('year','sitecode'),
                          allow.cartesian=T, all.x=TRUE)

us_map_w_data_q66[,prev_missing := is.na(prev)]
center_q66 = merge(as.data.table(centers), unique(us_map_w_data_q66[,.(year,id,prev_missing)]),
                   by=c('id'))

ggplot() +
  geom_map(data=us_map_w_data_q66[id!="DC"], map=us_map, aes(x=long, y=lat, map_id=id), color="white", size=0.5) +
  geom_map(data=us_map_w_data_q66[id!="DC"], map=us_map, aes(fill=prev, color=!prev_missing, map_id=id)) +
  geom_text(data=center_q66, aes(label=id, x=x, y=y, color=!prev_missing), size=4) + coord_map() + theme_bw() +
  labs(x=NULL, y=NULL, title="LGB Identity Prevalence\nby State and Year") + 
  theme(plot.title=element_text(face="bold", hjust=0, size=20)) + 
  theme(panel.border=element_blank()) + 
  theme(panel.grid=element_blank()) + 
  theme(axis.ticks=element_blank()) +
  theme(strip.background=element_blank()) + 
  theme(axis.text=element_blank()) + 
  facet_wrap(~year, ncol = 1) +
  theme(strip.text=element_text(face="bold", hjust=0, size=18)) +
  scale_fill_gradient(na.value="white") +
  scale_colour_manual(values = c("white", "black"), breaks = c(TRUE, FALSE)) +
  guides(color=FALSE) +
  theme(legend.title = element_blank())
```

Example LOOCV
```{r, fig.width=10}
loocv_state_years_w_data = public_w_states[year==2017, .(
  any_q66_data = max(ifelse(!is.na(q66),1,0))
), by = .(sitecode)]

loocv_state_years_w_data[, category := "Training Data"]
loocv_state_years_w_data[sitecode == "VT", category := "Evaluation Data"]
loocv_map_data = merge(us_map, loocv_state_years_w_data, by.x='id', by.y='sitecode', all.x=T)
loocv_map_data[is.na(category), category := "Missing Data"]


ggplot() +
  geom_map(data=us_map[id!="DC"], map=us_map, aes(x=long, y=lat, map_id=id), color="white", size=0.5) +
  geom_map(data=loocv_map_data[id!="DC"], map=us_map, aes(fill=category, map_id=id)) +
  geom_text(data=centers, aes(label=id, x=x, y=y), color="white", size=4) + coord_map() + theme_minimal() +
  labs(x=NULL, y=NULL, title="LOOCV Example:\nEvaluating VT Sexual Identity Prevalence Prediction") + 
  theme(plot.title=element_text(face="bold", hjust=0, size=14)) + 
  theme(panel.border=element_blank()) + 
  theme(panel.grid=element_blank()) + 
  theme(axis.ticks=element_blank()) +
  theme(strip.background=element_blank()) + 
  theme(axis.text=element_blank()) + 
  scale_fill_manual(values=c("#F44336", "gray", "#43A047", "black", "#00ACC1", "#AB47BC")) +
  labs(fill="Data Category")

```