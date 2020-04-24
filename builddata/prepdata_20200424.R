#######################################
# Saving Lives with Data 2
# Spring 2020
# Michele Claibourn

# 1. Read in displacement and conflict
# a.  Update displacement data to contain all relevant country-years
# b.  Filter for relevant observations/outcomes based on presence of conflict
# 2. Join additional data sources 
# 4. Begin to examine (each team submits a script diving into key variables)
#######################################

library(tidyverse)
library(readxl)
library(VIM)

#......................................
# 1. Read in displacement and conflict ----
# a. UNHCR as displacement source
flow <- read_csv("UNHCR/UN_Flow_By_CountryYear.csv")
flow <- flow %>% rename(iso = origincountry)  %>% # country var not changed
  select(-X1)
stock <- read_csv("UNHCR/UN_Total_Displaced.csv")
stock <- stock %>% rename(iso = origincountry)  %>% # country var not changed
  select(-X1)
span <- read_csv("UNHCR/UN_Span_Data.csv")
span <- span %>% select(-X1)

# combine these
disp <- full_join(stock, flow, by = c("iso", "year")) %>% 
  left_join(span, by = "iso")
# remove various/unknown
disp <- disp %>% filter(!(iso %in% c("Various/Unknown", "Stateless")))

# UPDATED: Expand to include rows for all country-years 
disp_full <- complete(disp, year, nesting(iso,span),  fill = list(size = 0, flow = 0))

# UPDATED: Work with 2000-2018 data
disp2000 <- disp_full %>% filter(year > 1999)


# b. Episodes of violence
# from prio: also includes  foreign intevention 
conflict <- read_csv("conflict/type_of_conflict.csv") 
# UPDATED: keep civil war conflict only and post-1999 only (to match displacement)
civcon <- conflict %>% filter(civilwar == 1 & year > 1999)
# UPDATED: keep only post-1999, all cases
allcon <- conflict %>% filter(year > 1999)


# 2. Generate relevant observations/outcomes ----
# keep displacement data only for countries present in civil war conflict countries 
# use only 2000-2018
disp_conflict <- disp2000 %>% filter(iso %in% civcon$iso) %>% 
  arrange(iso, year)

# same thing, but all conflict cases
disp_conflict2 <- disp2000 %>% filter(iso %in% allcon$iso) %>% 
  arrange(iso, year)

# Checks
# how many countries remain?
n_distinct(disp_conflict$iso) # 54 countries
n_distinct(disp_conflict2$iso) # 58 countries
setdiff(distinct(disp_conflict2, iso), distinct(disp_conflict, iso)) # difference
# currently choosing to go with civil war match

# join conflict to these displacement counts
disp_conflict <- left_join(disp_conflict, conflict, by = c("iso", "year"))
summary(disp_conflict)

# set missing conflict variables to 0 (no conflict)
disp_conflict <- disp_conflict %>% 
  replace(is.na(.), 0)

# disp_conflict is the starting point for our data set
rm(conflict, civcon, allcon, disp_conflict2, disp, flow, span, stock)


#......................................
# 3. Join additional data sources ----

# a. polity, world development
# NOTE: polity updated to embed missing codes
polity <- read_csv("polity_wdi/data_polity_processed.csv")
# NOTE: only need polity2 and dummy indicators for missing
# (if intent was to use other included vars fragment:exconst, change this; prior:regtrans largely missing)
polity <- polity %>% select(iso, year, country, polity2, interruption, anarchy)

wdi <- read_csv("polity_wdi/data_wdi_processed.csv")
# UPDATED: fix formatting of variable
wdi <- wdi %>% mutate(agri_employment_percent = as.numeric(agri_employment_percent))

df <- left_join(disp_conflict, polity, by = c("iso", "year")) %>% 
  rename(country_polity = country) %>% 
  left_join(wdi, by = c("iso", "year")) %>% 
  rename(country_wdi = country)


# b. world bank
wbd <- read_csv("wbd/WBD.csv")
wbd <- wbd %>%select(-c(name, inc_per, unemp)) # inc_per and unemp more than 50% missing in merged data, remove
gini <- read_csv("wbd/gini_cleaned.csv") # gini more than 70% missing in merged data, ignore

df <- left_join(df, wbd, by = c("iso", "year")) %>% 
  rename(gdp_wbd = gdp)


# c. political terror scale
pts <- read_excel("pts/PTS-2019.xlsx")
pts <- pts %>% rename(iso = WordBank_Code_A) %>% 
  rename_all(.funs = tolower) %>% 
  select(iso, year, region:pts_a, pts_s:na_status_a, na_status_s) %>% 
  mutate_at(c("pts_a", "pts_s"), as.numeric)

df <- left_join(df, pts, by = c("iso", "year"))


# d. food security
food <- read_csv("foodsecurity/food.csv")
food <- food %>% 
  select(iso, area, year, year_code, prev_undernourish, num_undernourish, val_foodimp, food_supply, everything())

df <- left_join(df, food, by = c("iso", "year")) 


names(df) # check
df <- df %>% mutate(iso3 = factor(iso)) # for use when a factor is needed
df <- as.data.frame(df) # to make it easier to use with VIM

rm(food, gini, polity, wbd, wdi, pts, food)
save.image("displacement.rdata")

# df is the key dataset to work with


#......................................
# 4. Begin to examine ----
# Start your own script beginnig with this and focusing on the key variables for which you are responsible
load("displacement.rdata")
summary(df)

# a. wdi data: key variables (Dheer and Patrick)
aglist <- c("agri_percent_land_area", "agri_forestry_fishing_percent_GDP", "agri_employment_percent", "imports_percent_GDP")
summary(df[,aglist])
# aggregation plot
aggr(df[, aglist], numbers=TRUE, prop = c(TRUE, FALSE))

# Spinogram
spineMiss(df[, c("year", "agri_percent_land_area")]) 
spineMiss(df[, c("iso3", "agri_percent_land_area")]) 


# b. wbd data: key variables (set 1, Austin and Thomas)
wbdlist <- c("net_exp", "inf_mor", "gdp_pc")
summary(df[,wbdlist])
# Aggregation plot
aggr(df[, wbdlist], numbers=TRUE, prop = c(TRUE, FALSE))

# Spinogram
spineMiss(df[, c("year", "net_exp")])  
spineMiss(df[, c("iso3", "net_exp")])


# c. wbd+pts: key variables (set 2, Chirag and Connor)
wbdptslist <- c("aid", "und_nor", "pts_a")
summary(df[,wbdptslist])
# Aggregation plot
aggr(df[, wbdptslist], numbers=TRUE, prop = c(TRUE, FALSE))

# Spinogram
spineMiss(df[, c("year", "aid")])  
spineMiss(df[, c("iso3", "aid")])


# c. food security: key variables (Jackie and Stearns)
fslist <- c("prev_undernourish", "val_foodimp", "food_supply") 
summary(df[,fslist])
# Aggregation plot
aggr(df[, fslist], numbers=TRUE, prop = c(TRUE, FALSE))

# Spinogram
spineMiss(df[, c("year", "prev_undernourish")])  
spineMiss(df[, c("iso3", "prev_undernourish")])


