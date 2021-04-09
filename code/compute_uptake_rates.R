rm (list = ls())

library(data.table)
library(ggplot2)
library(tidycensus)

####################################
## Prepare ACS Population Data
####################################

## FIPS MAP ##
fips <- as.data.table(tidycensus::fips_codes)
fips <- unique(fips[,.(state_name, state_code)])
fips <- fips[, state_code:=as.numeric(state_code)]

## READ 2019 ACS Single-Year Population ##
df <- fread("data/usa_00024.csv")
df <- df[GQ%in%c(1, 2, 5)] ## Remove institutionalized populations

## Assign race/ethnicity
df <- df[HISPAN%in%c(1, 2, 3, 4), race_grp:="Hispanic"]
df <- df[is.na(race_grp) & RACE==1, race_grp:="White"]
df <- df[is.na(race_grp) & RACE==2, race_grp:="Black"]
df <- df[is.na(race_grp) & RACE==3, race_grp:="American Indian or Alaska Native"]
df <- df[is.na(race_grp) & RACE%in%c(4, 5), race_grp:="Asian"]
df <- df[is.na(race_grp) & RACE%in%c(7, 8, 9), race_grp:="Other"]
df <- df[is.na(race_grp) & RACED%in%c(630, 680, 681, 682, 685, 689, 690, 699), race_grp:="Native Hawaiian or Other Pacific Islander"]
df <- df[is.na(race_grp), race_grp:="Asian"]

## Merge state name
df <- merge(df, fips, by.y = "state_code", by.x="STATEFIP")

## Make a dt with total population age 16+ by state and race
pops_16 <- df[AGE>=16]
pops_16 <- pops_16[, state_pop:=sum(PERWT, na.rm=T), by = c("state_name", "race_grp")]
pops_16 <- unique(pops_16[,.(state_name, race_grp, state_pop)])

####################################
## Compute "expected" share
####################################

## Read in extracted data on age distribution of vaccinations from state websites
vax_age <- fread("./data/age_distribution_Mar31.csv")[,note:=NULL]
vax_age <- vax_age[`Age End`>=16 | is.na(`Age End`)] # Only 16+ are eligible
vax_age <- vax_age[, id:=1:.N, by="state_name"] # Row id by state
vax_age <- vax_age[`Age Start`<16, `Age Start`:=16] # Only 16+ are eligible
vax_age <- vax_age[, pct_vaccinated:=pct_vaccinated/sum(pct_vaccinated, na.rm=T), by = "state_name"]

out <- NULL
for (s in unique(vax_age$state_name[!is.na(vax_age$`Age Start`)])) {
  temp <- df[state_name==s]  
  vax_temp <- vax_age[state_name==s]
  for (i in 1:nrow(vax_temp)) {
    temp <- temp[AGE>=vax_temp$`Age Start`[vax_temp$id==i] & AGE<=vax_temp$`Age End`[vax_temp$id==i], `Age Start`:=vax_temp$`Age Start`[vax_temp$id==i]]
    temp <- temp[AGE>=vax_temp$`Age Start`[vax_temp$id==i] & AGE<=vax_temp$`Age End`[vax_temp$id==i], `Age End`:=vax_temp$`Age End`[vax_temp$id==i]]
  }
  temp <- temp[!is.na(`Age Start`)]
  temp <- temp[, pop_race_age:=sum(PERWT, na.rm=T), by = c("state_name", "Age Start", "Age End", "race_grp")]
  temp <- unique(temp[,.(pop_race_age, `Age Start`, `Age End`, race_grp, state_name)])
  temp <- temp[, pop_share:=pop_race_age/sum(pop_race_age, na.rm=T), by = c("state_name", "Age Start", "Age End")]
  out <- rbind(out, temp, fill = T)
}

for (s in unique(vax_age$state_name[is.na(vax_age$`Age Start`)])) {
  temp <- df[state_name==s]  
  vax_temp <- vax_age[state_name=="All"]
  for (i in 1:nrow(vax_temp)) {
    temp <- temp[AGE>=vax_temp$`Age Start`[vax_temp$id==i] & AGE<=vax_temp$`Age End`[vax_temp$id==i], `Age Start`:=vax_temp$`Age Start`[vax_temp$id==i]]
    temp <- temp[AGE>=vax_temp$`Age Start`[vax_temp$id==i] & AGE<=vax_temp$`Age End`[vax_temp$id==i], `Age End`:=vax_temp$`Age End`[vax_temp$id==i]]
  }
  temp <- temp[!is.na(`Age Start`)]
  temp <- temp[, pop_race_age:=sum(PERWT, na.rm=T), by = c("state_name", "Age Start", "Age End", "race_grp")]
  temp <- unique(temp[,.(pop_race_age, `Age Start`, `Age End`, race_grp, state_name)])
  temp <- temp[, pop_share:=pop_race_age/sum(pop_race_age, na.rm=T), by = c("state_name", "Age Start", "Age End")]
  out <- rbind(out, temp, fill = T)
}

out <- merge(out, vax_age, by = c("state_name", "Age Start", "Age End"), all.x=T)
temp <- out[is.na(pct_vaccinated)]
temp <- temp[, c("pct_vaccinated", "num_vaccinated", "id"):=NULL]
vax_age_nat <- vax_age[state_name=="All"]
vax_age_nat <- vax_age_nat[, state_name:=NULL]
temp <- merge(temp, vax_age_nat, by = c("Age Start", "Age End"))
out <- out[!is.na(pct_vaccinated)]
out <- rbind(out, temp)
out <- out[, pop_share_agg:=sum(pop_share*pct_vaccinated), by = c("race_grp", "state_name")]
out <- out[, pop_weighted:=sum(pop_race_age*pct_vaccinated), by = c("race_grp", "state_name")]
out <- unique(out[,.(state_name, race_grp, pop_share_agg, pop_weighted)])

write.csv(out, "prepped/population_share_vax.csv", na = "", row.names = F)

####################################
## Compute "observed" share
####################################

vax_race <- fread("data/race_ethnic_share_Mar31.csv")
vax_race <- vax_race[Location=="Alabama", Hispanic:="NR"] ## 85% missingness too high
vax_race <- vax_race[Location=="Alabama", `Race Categories Include Hispanic Individuals`:=""] ## 85% missingness too high
vax_race <- vax_race[,source:=NULL]
setnames(vax_race, "Location", "state_name")
vax_race <- vax_race[, lapply(.SD, as.numeric), by = c("state_name", "Race Categories Include Hispanic Individuals")]
vax_race <- melt(vax_race, id.vars = c("state_name", "Race Categories Include Hispanic Individuals", "Hispanic Rescaled"))
setnames(vax_race, "variable", "race_grp")

## Adjust separate hispanic reporting for missingness
vax_race <- vax_race[race_grp=="Unknown Race", unknown_race:=value]
vax_race <- vax_race[, unknown_race:=mean(unknown_race, na.rm=T), by = "state_name"]
vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes" & race_grp=="Unknown Ethnicity", unknown_eth:=value]
vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes", unknown_eth:=mean(unknown_eth, na.rm=T), by = "state_name"]
vax_race <- vax_race[!is.na(unknown_eth) & `Race Categories Include Hispanic Individuals`=="Yes" & race_grp=="Hispanic" & `Hispanic Rescaled`!=1, value_adj:=value/(1-unknown_eth)]
vax_race <- vax_race[is.na(value_adj) & `Race Categories Include Hispanic Individuals`=="Yes" & race_grp=="Hispanic", value_adj:=value]

vax_race <- vax_race[race_grp!="Unknown Race" & race_grp!="Unknown Ethnicity"]
vax_race <- vax_race[race_grp!="Other"] # Other unrealistically high, consider these data unknown for computing uptake rates

vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes" & race_grp=="Hispanic", fixed_hisp:=value_adj]
vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes", fixed_hisp:=mean(fixed_hisp, na.rm=T), by = "state_name"]
vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes" & race_grp!="Hispanic", value_adj:=value/sum(value, na.rm=T), by = "state_name"]
vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes" & race_grp%in%c("Black", "White"), bw_share:=value/sum(value, na.rm=T), by = "state_name"]
vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes", bw_hispanic_overlap:=fixed_hisp*bw_share]
vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="Yes" & !is.na(bw_hispanic_overlap), value_adj:=value_adj-bw_hispanic_overlap]
vax_race <- vax_race[, c("fixed_hisp", "bw_share", "bw_hispanic_overlap"):=NULL]

vax_race <- vax_race[`Race Categories Include Hispanic Individuals`=="", value_adj:=value/sum(value, na.rm=T), by = "state_name"]

vax_race <- merge(vax_race, out, by = c("state_name", "race_grp"), all.x=T)
vax_race <- vax_race[!is.na(value_adj), pop_share_agg_adj:=pop_share_agg/sum(pop_share_agg, na.rm=T), by = "state_name"]

## Uptake rates: observed/expected
vax_race <- vax_race[, effective_demand:=value_adj/pop_share_agg_adj]

write.csv(vax_race, "prepped/effective_demand_weights_state.csv", na = "", row.names = F)

## Aggregate to census division
regions <- fread("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")
setnames(regions, "State", "state_name")

vax_race <- merge(vax_race, regions, by = "state_name")
vax_race <- merge(vax_race, pops_16, by = c("state_name", "race_grp"))

division <- copy(vax_race)
division <- division[!is.na(value_adj)]
division <- division[, weight_actual_division:=sum(effective_demand*pop_weighted, na.rm=T)/sum(pop_weighted, na.rm=T), by = c("Division", "race_grp")]
division <- unique(division[,.(race_grp, weight_actual_division, Division)])

write.csv(division, "prepped/effective_demand_weights.csv", na = "", row.names = F)
write.csv(pops_16, "prepped/pops_16.csv", na = "", row.names = F)
write.csv(vax_race[,.(state_name, race_grp, value_adj)], "prepped/share_race_vax.csv", na = "", row.names = F)
