#######################################
# Saving Lives with Data 2
# Spring 2020
# Michele Claibourn

# 1. Read in displacement and conflict
# 2. Generate relevant observations/outcomes
# 3. Join additional data sources 
# 4. Begin to examine (incomplete)
# 5. Begin to examine relationships (not done)
#######################################

library(tidyverse)

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


# b. Episodes of violence
# from mepv:
conflict1 <- read_csv("conflict/episodes_of_political_violence.csv")
# keep civil war conflict only and post-1960 only (to match displacement)
conflict1 <- conflict1 %>% filter(civwar == 1, year > 1959) %>% select(-intwar)
# from prio: also includes  foreign intevention 
conflict2 <- read_csv("conflict/type_of_conflict.csv") 
# keep civil war conflict only and post-1960 only (to match displacement)
conflict2 <- conflict2 %>% filter(civilwar == 1, year > 1959)

# 2. Generate relevant observations/outcomes ----
# keep displacement data only for countries present in civil war conflict countries 
# (keep all years initially, as displacement could continue after or begin prior?)
# using confict2 (prio)
disp_conflict <- disp %>% filter(iso %in% conflict2$iso) %>% 
  arrange(iso, year)

# Checks
# how many countries remain?
n_distinct(disp_conflict$iso) # 88 countries

# join conflict to these displacement counts
disp_conflict <- left_join(disp_conflict, conflict2, by = c("iso", "year"))

# are there non-consecutive series by country?
tmp <- disp_conflict %>% group_by(iso) %>% mutate(yearlag = year - lag(year))
tmp <- tmp %>% filter(yearlag>1) # show country that appears again after a gap in years
# yes, many
# need to determine if we are keeping all years, 
#   only single consecutive series of years 
#   only serie of years in which civil war is present,
#   something else....
# for the moment, keeping all years

# non-civil war years with displacement?
tmp <- disp_conflict %>% 
  mutate(civilwar = if_else(is.na(civilwar), 0, civilwar)) %>% 
  group_by(iso) %>% 
  mutate(cw_change = if_else(civilwar == lag(civilwar), 0, 1), # 1=change
         cw_change = if_else(is.na(cw_change), 0, cw_change), # first appearance=0 if na
         sumchange = sum(cw_change)) # does country experience change in civwar presence

# countries experience change in civil war presence? 
tmp %>% filter(sumchange > 0) %>% ungroup() %>% summarize(n_distinct(iso))
#   76 (of 88) have obs with and without civil war present

# years of country presence in displacement without civil war present
tmp <- tmp %>% summarize(numyears = n(), cwyears = sum(civilwar))
# remove country obs with no presence of civil war in this time frame
#   these were present in conflict2, but not during same years as they 
#   they are present in disp...

disp_conflict <- disp_conflict %>% 
  mutate(civilwar = if_else(is.na(civilwar), 0, civilwar),
         intervention = if_else(is.na(intervention), 0, intervention)) %>% 
  group_by(iso) %>% 
  mutate(civwaryrs = sum(civilwar),
         dispyrs = n()) %>% 
  filter(civwaryrs != 0) %>% 
  ungroup() %>% 
  select(-interstate)

# disp_conflict is the starting point for our data set
rm(conflict1, conflict2, disp, flow, span, stock, tmp)


#......................................
# 3. Join additional data sources ----
# a. food security
# need guidance on which is the right file to join
food <- read_csv("foodsecurity/dfyearwide.csv")
food <- food %>% rename(iso = ISO) # rename iso for consistency

# creating new dataframe: df
df <- left_join(disp_conflict, food, by = c("iso", "year"))

# b. polity, world development
polity <- read_csv("polity_wdi/data_polity_processed.csv")
wdi <- read_csv("polity_wdi/data_wdi_processed.csv")

df <- left_join(df, polity, by = c("iso", "year")) %>% 
  left_join(wdi, by = c("iso", "year"))

# c. world bank
wbd <- read_csv("wbd/WBD.csv")
gini <- read_csv("wbd/gini_cleaned.csv")

df <- left_join(df, wbd, by = c("iso", "year")) %>% 
  left_join(gini, by = c("iso", "year"))

names(df)

rm(food, gini, polity, wbd, wdi)
save.image("displacement.rdata")


#......................................
# 4. Begin to examine ----

# load("displacement.rdata") # this will load these data saved above
                             # a good place to start new work once you 
                             # understand how this dataframe was generated.
library(VIM)

summary(df)

# a. some examples for missingness
# See Kuhn and Johnson chapter http://www.feat.engineering/handling-missing-data.html (from syllabus) for more

# Aggregation plot: barplot of proportion missing, frequency of missing combinations
# Kuhn and Johnson (above) call this a co-occurrence plot
#    too many variables to easily see at once; picking a few
varlist <- c("size", "political_stability", "gdp.x", "food_production", "fragment", "polity",
             "aid", "unemp", "gdp_pc", "gini")
aggr(df[, varlist], numbers=TRUE, prop = c(TRUE, FALSE))

# Spinogram: proportion of missing values in second by values of first
#   is missingness related to another variable
df <- as.data.frame(df) # VIM seems to struggle with tbls
spineMiss(df[, c("gdp_pc", "aid")]) 
# higher missingness with higher gdp_pc
spineMiss(df[, c("gdp_pc", "food_production")]) 
# no pattern

# b. do more exploration around missingnesss -- there's a lot!
#   some may represent something analagous to zero values or absence
#   some may be missing systematically, as function of other variables
#   see Kuhn and Johnson above for a bit more on MCAR, MAR, NMAR


#......................................
# 5. Begin to examine relationships among variables ----

# For Friday, begin exploring the dataset further to inform model-building; 
# consider the same types of exploration we pursued in problem sets 1 and 2
# earlier in the year. Feel free to begin playing with some models if you'd
# like. Go ahead and save a new R script (or markdown file) with your work,
# both in understanding missingness and exploring relationships, and feel 
# free to work in teams if time permits or alone if that's easier.
