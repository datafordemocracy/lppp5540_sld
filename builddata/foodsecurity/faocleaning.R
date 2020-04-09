library(tidyverse)
library(readxl)
library(dplyr)

#reading in food security data
df <- read.csv("Food_Security_Data_E_All_Data_(Normalized).csv")

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
dfyearwide <- reshape(dfyear, 
                  timevar = "Item", 
                  idvar = c("Area", "Year", "Year.Code", "Area.Code"),
                  drop = c("Unit", "Item.Code", "Element", "Element.Code", "Flag"),
                  direction = "wide")

df3yearwide <- reshape(df3year, 
                      timevar = "Item", 
                      idvar = c("Area", "Year", "Year.Code", "Area.Code"),
                      drop = c("Unit", "Item.Code", "Element", "Element.Code", "Flag"),
                      direction = "wide")

#merging iso3s
dfyearwide <- merge(dfyearwide, iso3codes, by = "Area.Code")
df3yearwide <- merge(df3yearwide, iso3codes, by = "Area.Code")

#renaming variables
newnames <- read_excel("FAO_newnames.xlsx")  
names(dfyearwide)[1:27] <- newnames$newname

# writing to csvs
write_csv(dfyearwide, path = "dfyearwide.csv") 
write_csv(df3yearwide, path = "df3yearwide.csv") 