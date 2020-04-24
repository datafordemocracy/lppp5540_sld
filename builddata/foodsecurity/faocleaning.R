library(tidyverse)
library(dplyr)
library(VIM)

#reading in food security data
df <- read.csv("FAOSTAT_data_4-17-2020.csv")

# Trying to get the missing values of the dataset by using the flags for "No Value" and "Not Recorded"
countriesmis <- df %>% filter(df$Flag == "NR" | df$Flag == "NV")
unique(countriesmis$Area)

#Finding the countries where data is recorded
countriespresent <- df %>% filter(df$Flag != "NR" & df$Flag != "NV")
unique(countriesmis$Area)

#Vector of unique countries in each dataset
misingdata <- unique(countriesmis$Area)
presentdata <- unique(countriespresent$Area)

#countries where there is complete data
complete <- setdiff(presentdata, misingdata)

#countries where there is absolutely no data for prevalence of undernourishment, moderate to severe food insecurity
nodata <- setdiff(misingdata, presentdata)

#countries with incomplete data
common <- intersect(presentdata, misingdata)

write.table(common, "incompletecountries.txt", sep="\t")
write.table(nodata, "nodatacountries.txt", sep="\t")
write.table(complete, "completecountries.txt", sep="\t")

# Back to the code I made before
# reading in iso3 codes and reducing the dataset to be merged
iso3codes <- read.csv("FAOSTAT_data_4-1-2020.csv")
iso3codes$Area.Code <- iso3codes$Country.Code
myvars <- c("Area.Code", "ISO3.Code")
iso3codes <- iso3codes[myvars]

# splitting the data into two frames

#this frame includes SDG indicators measured over a three year average 
df3year <- subset(df, Year.Code >= 19992000)

#this frame includes SDG indicators measured yearly 
dfyear <- subset(df, Year.Code < 19992000)

# reshaping the data to wide datasets to get country-year as unit of observation
dfwide <- pivot_wider(dfyear,
                      id_cols = c("Area", "Year", "Year.Code", "Area.Code"),
                      names_from = "Item",
                      values_from = "Value")
df3wide <- pivot_wider(df3year,
            id_cols = c("Area", "Year", "Year.Code", "Area.Code"),
            names_from = "Item",
            values_from = "Value")

#merging iso3s
dfyearwide <- merge(dfyearwide, iso3codes, by = "Area.Code")
df3wide <- merge(df3yearwide, iso3codes, by = "Area.Code")

# writing to csvs
write_csv(dfyearwide, path = "dfyearwide.csv") 
write_csv(df3yearwide, path = "df3yearwide.csv") 