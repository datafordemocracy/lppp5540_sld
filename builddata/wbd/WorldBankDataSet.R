#Loading Libraries
library(dplyr)
library(readr)
library(readxl)
library(tidyverse)

#reading World Bank Data
WBD <- read_excel("WBD.xlsx")

#Remove Time Code Column
WBD <- WBD[-4] 

#Adding in New Titles for clarity
newtitles <- read_excel("newtitles.xlsx") 
names(WBD) <- newtitles$Names

#Reclassifing Variables
WBD$`Country Name` <- as.factor(WBD$`Country Name`)
WBD$`ISO3 COde` <- as.factor(WBD$`ISO3 Code`)
WBD$Year <- as.factor(WBD$Year)
WBD$`Aid Received (USD)` <- as.numeric(WBD$`Aid Received (USD)`)
WBD$`Infant Mortality Rate` <- as.numeric(WBD$`Infant Mortality Rate`)
WBD$`Undernourishment Percentage` <- as.numeric(WBD$`Undernourishment Percentage`)
WBD$`Infant Mortality Rate` <- as.numeric(WBD$`Infant Mortality Rate`) 
WBD$`Income Percent held by richest 10%` <- as.numeric(WBD$`Income Percent held by richest 10%`) 
WBD$`Unemployment Percentage` <- as.numeric(WBD$`Unemployment Percentage`) 
WBD$`Net Exports (Percent GDP)` <- as.numeric(WBD$`Net Exports (Percent GDP)`) 
WBD$`Exports of Goods and Services (USD)` <- as.numeric(WBD$`Exports of Goods and Services (USD)`)
WBD$`GDP per capita (USD)` <- as.numeric(WBD$`GDP per capita (USD)`)
WBD$`GDP (USD)` <- as.numeric(WBD$`GDP (USD)`)

#Removing the last 5 rows of blank data
WBD <- WBD[-c(13021, 13022, 13023, 13024, 13025), ]

#Removing all years 2019
WBD <- subset(WBD, Year != 2019)

#Removing a duplicate ISO3 column which was added somehow
WBD <- WBD[-13] 

view(WBD)

write_csv(WBD, path = "WBD.csv")
