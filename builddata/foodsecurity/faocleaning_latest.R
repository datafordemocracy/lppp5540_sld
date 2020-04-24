library(tidyverse)
library(dplyr)
library(VIM)
library(readxl)

#reading in food security data
df <- read.csv("FAOSTAT_data_4-22-2020.csv", na.strings=c("","NA"))
df <- as.data.frame(sapply(df, gsub, pattern = "<|>", replacement = ""))
df <- df %>% mutate(Value = as.numeric(as.character(Value)),
                    Year.Code = as.numeric(as.character(Year.Code)))
df <- as.data.frame(df)


# Trying to get the missing values of the dataset by using the flags for "No Value" and "Not Recorded"
countriesmis <- df %>% filter(df$Flag == "NR" | df$Flag == "NV")
unique(countriesmis$Area)

#Finding the countries where data is recorded
countriespresent <- df %>% filter(df$Flag != "NR" & df$Flag != "NV")
unique(countriesmis$Area)

#Vector of unique countries in each dataset
misingdata <- unique(countriesmis[c("Area", "Year")])
presentdata <- unique(countriespresent[c("Area", "Year")])

#countries where there is complete data
complete <- setdiff(presentdata, misingdata)

#countries where there is absolutely no data for prevalence of undernourishment, moderate to severe food insecurity
nodata <- setdiff(misingdata, presentdata)

#countries with incomplete data
common <- intersect(presentdata, misingdata)

write.table(common, "incompletecountries.txt", col.names = c("area", "year_code"), sep="\t")
write.table(nodata, "nodatacountries.txt", col.names = c("area", "year_code"), sep="\t")
write.table(complete, "completecountries.txt", col.names = c("area", "year_code"), sep="\t")

# Back to the code I made before
# reading in iso3 codes and reducing the dataset to be merged
iso3codes <- read.csv("FAOSTAT_data_4-1-2020.csv")
iso3codes$Area.Code <- iso3codes$Country.Code
myvars <- c("Area.Code", "ISO3.Code")
iso3codes <- iso3codes[myvars]

# splitting the data into two frames

#this frame includes SDG indicators measured over a three year average 
df3year <- subset(df, Year.Code >= 19992000)
dfyear <- subset(df, Year.Code <= 19992000)

unique(dfyear$Year)
# reshaping the data to wide datasets to get country-year as unit of observation
df3wide <- pivot_wider(df3year,
            id_cols = c("Area", "Year", "Year.Code", "Area.Code"),
            names_from = "Item",
            values_from = "Value")

dfwide <- pivot_wider(dfyear,
                      id_cols = c("Area", "Year", "Year.Code", "Area.Code"),
                      names_from = "Item",
                      values_from = "Value")

#merging iso3s
dfwideiso <- merge(dfwide, iso3codes, by = "Area.Code")
df3wideiso <- merge(df3wide, iso3codes, by = "Area.Code")

dfwideiso <- as.data.frame(dfwideiso)
df3wideiso <- as.data.frame(df3wideiso)

#renaming variables
newnames <- read_excel("FAO_newnames_Revised.xlsx")
names(dfwideiso) <- newnames$newname

newnames3year <- read_excel("FAO_newnames3year_Revised.xlsx")
names(df3wideiso) <- newnames3year$newname

newyears <- read_csv("newyears.csv")
df3wideiso <- left_join(df3wideiso, newyears, by = "year_code")
df3wideiso <- df3wideiso %>% mutate("year" = as.numeric(yearadjusted))
df3wideiso <- left_join(df3wideiso, dfwideiso, by = c("year" = "year", "area_code" = "area_code", "area" = "area", "iso" = "iso"))

colnames(df3wideiso)

food <- df3wideiso %>% select(year, year_code.x, area, iso, prev_undernourish, num_undernourish, prev_severe, 
                          prev_moderate, num_severe, num_moderate, val_foodimp, food_supply)

# writing to csvs
write_csv(food, path = "food.csv")

summary(df3wideiso$num_undernourish)


