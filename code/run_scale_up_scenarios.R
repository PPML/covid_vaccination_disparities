rm(list = ls())

library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)

regions <- fread("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")
regions <- regions[, geo_id:=tolower(`State Code`)]

## Census Tract Population Age-Race Structure
df <- fread("data/census_tract_final_long.csv")
df <- df[, c("tract", "county", "state_name"):=tstrsplit(x = NAME, split = ", ")]
fips_codes <- as.data.table(tidycensus::fips_codes)
df <- merge(df, fips_codes, by = c("county", "state_name"), all.x=T)
df <- df[county=="Doña Ana County", state:="NM"]
df <- df[county=="Doña Ana County", state_code:="35"]
df <- df[county=="Doña Ana County", county_code:="013"]
df <- df[county=="LaSalle Parish", state:="LA"]
df <- df[county=="LaSalle Parish", state_code:="22"]
df <- df[county=="LaSalle Parish", county_code:="059"]
df <- df[county=="Petersburg Borough", state:="AK"]
df <- df[county=="Petersburg Borough", state_code:="02"]
df <- df[county=="Petersburg Borough", county_code:="195"]

# Clean race/ethnicity
df <- df[race_grp_full == "Multiracial", race_grp_full:="Other"]
df <- df[race_grp_full == "Native Hawaiian or Pacific Islander", race_grp_full:="Native Hawaiian or Other Pacific Islander"]
df <- df[race_grp_full=="Latino", race_grp_full:="Hispanic"]


df <- df[, state_pop:=sum(estimate, na.rm=T), by = c("state_name", "race_grp_full")]
setnames(df, "race_grp_full", "race_grp")

## For this, using ages 15+ (no individual-level data available at tract level)
df <- df[`Age Start`>=15]
df <- df[`Age Start`==15, estimate:=estimate*.9] # Reduce population because 15 year olds are not yet eligible

## Merge in SVI
files <- list.files("data/svi_data/")

svi <- NULL
for (f in files) {
  temp <- fread(paste0("data/svi_data/", f))
  temp <- temp[,.(ST_ABBR, FIPS, RPL_THEMES)]
  svi <- rbind(svi, temp, fill = T)
}

df <- merge(df, svi, by.x = c("GEOID"), by.y = "FIPS") # This drops 219 census tracts with 0 population (and no SVI)

## Fill missing SVI with mean of the state
df <- df[RPL_THEMES>=0 & RPL_THEMES<=1, mean_svi_state:=sum(RPL_THEMES*estimate)/sum(estimate), by = c("state")]
df <- df[, mean_svi_state:=mean(mean_svi_state, na.rm=T), by = "state"]
df <- df[RPL_THEMES<0 | RPL_THEMES>1, RPL_THEMES:=mean_svi_state] # Replace missing SVI with state mean SVI

df <- df[, estimate:=sum(estimate, na.rm=T), by = c("GEOID", "state_name", "race_grp")]
df <- unique(df[,.(GEOID, race_grp, county, state_name, state_code, estimate, state, county_code, RPL_THEMES, ST_ABBR, state_pop)])
df <- merge(df, regions[,.(`State Code`, Region, Division)], by.y = "State Code", by.x="state")

# Access
state <- fread("prepped/effective_demand_weights_state.csv")
df <- merge(df, state[,.(race_grp, state_name, effective_demand)], by = c("race_grp", "state_name"), all.x=T)
division <- fread("prepped/effective_demand_weights.csv")
setnames(division, "weight_actual_division", "weight_actual")
df <- merge(df, division[,.(race_grp, Division, weight_actual)], by = c("race_grp", "Division"), all.x=T)
df <- df[is.na(effective_demand), effective_demand:=weight_actual]
df <- df[is.na(effective_demand), effective_demand:=1] # Other doesn't have weight, assign proportional to population
df <- df[, weight_actual:=NULL]
setnames(df, "effective_demand", "weight_actual")

# Vax Stats
con <- dbConnect(SQLite(), "~/COVID/vaccination_analysis/cdc-vaccination-history/cdc.db")
vax_stats <- as.data.table(dbReadTable(con, 'daily_reports'))
vax_stats <- vax_stats[, Date:=as.Date(Date)]
setnames(vax_stats, "LongName", "state_name")
vax_stats <- vax_stats[Date=="2021-04-01"]
vax_stats <- vax_stats[,.(state_name, `Administered_Dose1_Recip`)]
vax_stats <- vax_stats[, lapply(.SD, as.numeric), by = "state_name"]
vax_stats <- vax_stats[, doses:=`Administered_Dose1_Recip`]
vax_stats <- vax_stats[,.(doses, state_name)]
vax_stats <- vax_stats[state_name=="New York State", state_name:="New York"]

# Population 16+
state_pops <- fread("prepped/pops_16.csv")
vax_stats <- merge(vax_stats, state_pops, by = "state_name")
pop_share_vax <- fread("prepped/population_share_vax.csv")
vax_stats <- merge(vax_stats, pop_share_vax[,.(state_name, race_grp, pop_share_agg)], by = c("state_name", "race_grp"))

# Race
vax_race <- fread("prepped/share_race_vax.csv")
vax_stats <- merge(vax_stats, vax_race, by = c("state_name", "race_grp"), all.x=T)
vax_stats <- vax_stats[is.na(value_adj), pct_missing:=pop_share_agg]
vax_stats <- vax_stats[, pct_missing:=sum(pct_missing, na.rm=T), by = "state_name"]
vax_stats <- vax_stats[, value_adj:=value_adj*(1-pct_missing)]
vax_stats <- vax_stats[is.na(value_adj), value_adj:=pop_share_agg]
vax_stats <- vax_stats[, value_adj:=value_adj/sum(value_adj, na.rm=T), by = "state_name"]
vax_stats <- vax_stats[, race_doses:=doses*value_adj]
vax_stats <- vax_stats[race_doses>state_pop, add_doses:=race_doses-state_pop]
vax_stats <- vax_stats[, add_doses:=sum(add_doses, na.rm=T), by = "state_name"]
vax_stats <- vax_stats[race_doses>state_pop, exceeded:=1]
vax_stats <- vax_stats[race_doses>state_pop, race_doses:=state_pop]
vax_stats <- vax_stats[is.na(exceeded), second_pct:=value_adj/sum(value_adj), by = "state_name"]
vax_stats <- vax_stats[is.na(exceeded), race_doses:=race_doses+(second_pct*add_doses)]
vax_stats <- vax_stats[, vaccinated_pct:=(race_doses/state_pop)*100]
vax_stats <- vax_stats[,.(state_name, race_grp, vaccinated_pct)]

df <- merge(df, vax_stats, by = c("state_name", "race_grp"))

df <- df[, vaccinated:=estimate*vaccinated_pct/100]

## State allocation based on size of 16+ population
# alloc <- fread("prepped/pops_16.csv")
# alloc <- alloc[, state_pop:=sum(state_pop, na.rm=T), by = "state_name"]
# alloc <- unique(alloc[,.(state_pop, state_name)])
# alloc <- alloc[, state_pct:=state_pop/sum(state_pop)]
# 
# df <- merge(df, alloc, by = "state_name")
vax_history <- as.data.table(dbReadTable(con, 'daily_reports'))
vax_history <- vax_history[LongName=="New York State", LongName:="New York"]
vax_history <- vax_history[, Date:=as.Date(Date)]
vax_history <- vax_history[,.(Date, LongName, Administered_Dose1_Recip)]
setnames(vax_history, "LongName", "state_name")
vax_history <- vax_history[, l1:=lag(Administered_Dose1_Recip, n = 1), by = c("state_name")]
vax_history <- vax_history[, incident:=Administered_Dose1_Recip-l1]
vax_history <- vax_history[, smooth:=frollmean(incident, 7, align = "right"), by = c("state_name")]
vax_history <- vax_history[Date=="2021-04-01"]

real_backup <- copy(df)

## Start here for scenarios
df <- copy(real_backup)

geo <- "GEOID"

sim_data <- copy(df)

########################
#### SCENARIO FLAGS ####
########################

## Status Quo
## Equalized Uptake
## Equalized Uptake and Geographic Targeting

scenario_label <- "Equalized Uptake and Geographic Targeting"

if (scenario_label == "Status Quo") {
  print(paste0("Running ", scenario_label))
  sim_data <- sim_data[, wtd_pop:=ifelse(RPL_THEMES>=.75, estimate, estimate)]
}

if (scenario_label == "Equalized Uptake") {
  print(paste0("Running ", scenario_label))
  sim_data <- sim_data[, wtd_pop:=ifelse(RPL_THEMES>=.75, estimate, estimate)]
}

if (scenario_label == "Equalized Uptake and Geographic Targeting") {
  print(paste0("Running ", scenario_label))
  sim_data <- sim_data[, wtd_pop:=ifelse(RPL_THEMES>=.75, estimate*1.6, estimate*.8)]
}

## Compute tract allocation
sim_data <- sim_data[, geo_pop:=sum(wtd_pop, na.rm=T), by = c(geo, "state_name")]
sim_data <- sim_data[, pop_elig:=sum(estimate, na.rm=T), by = "state_name"]
sim_data <- sim_data[, geo_alloc:=geo_pop/pop_elig]
sim_data <- sim_data[, weighted_demand:=(estimate-vaccinated)*weight_actual]
sim_data <- sim_data[, race_age_pct:=weighted_demand/sum(weighted_demand, na.rm=T), by = c(geo, "state_name")]
sim_data <- sim_data[, state_race_elig:=sum(estimate, na.rm=T), by = c("state_name", "race_grp")]

## Fix Current Values ##
sim_data <- sim_data[, weight_actual_current:=weight_actual]
sim_data <- sim_data[, original_vaccinated:=vaccinated]

backup <- copy(sim_data)

time_to_cap <- 7*6

out <- NULL
for (s in unique(sim_data$state_name)) {
  print(s)
  supply <- vax_history$smooth[vax_history$state_name==s & vax_history$Date=="2021-04-01"]
  sim_data <- backup[state_name==s]
  counter <- 1
  for (i in as.list(seq.Date(as.Date("04/01/2021", format="%m/%d/%Y"),as.Date("9/01/2021", format="%m/%d/%Y"), "days"))) {
    sim_data <- sim_data[, daily_vax:=supply*geo_alloc*race_age_pct]
    sim_data <- sim_data[, vaccinated:=ifelse(vaccinated+daily_vax<=estimate*(.95), vaccinated+daily_vax, estimate*(.95))]
    temp <- copy(sim_data)
    temp <- temp[, daily_vax:=sum(daily_vax, na.rm=T), by = c("race_grp", "state_name")]
    temp <- temp[, vaccinated:=sum(vaccinated, na.rm=T), by = c("race_grp", "state_name")]
    temp <- unique(temp[,.(race_grp, daily_vax, vaccinated, state_name, state_race_elig, state_pop, pop_elig)])
    temp <- temp[, day:=i]
    out <- rbind(out, temp, fill = T)
    if (counter <= time_to_cap) {
      if (scenario_label == "Equalized Uptake" | scenario_label == "Equalized Uptake and Geographic Targeting") {
        sim_data <- sim_data[, weight_actual_current:=weight_actual_current+(((1-weight_actual)/time_to_cap))]
        sim_data <- sim_data[, weighted_demand:=(estimate-original_vaccinated)*weight_actual_current]
        sim_data <- sim_data[, race_age_pct:=weighted_demand/sum(weighted_demand, na.rm=T), by = c(geo, "state_name")]
      }
    }
    if (counter == 42) {
      sim_data <- sim_data[, geo_pop:=sum(estimate, na.rm=T), by = c(geo, "state_name")]
      sim_data <- sim_data[, geo_alloc:=geo_pop/pop_elig]
    }
    counter <- counter+1
  }
}

out <- out[, scenario:=paste0(scenario_label)]

write.csv(out, paste0("results/", scenario_label, ".csv"), na = "", row.names = F)


