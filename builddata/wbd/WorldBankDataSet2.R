library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyverse)
#loading necessary libraries

WorldBankData <- read_excel("WorldBankData.xlsx")
#reading in the data

WorldBankData <- WorldBankData[-4] 
# remove time code

WorldBankData <- rename(WorldBankData, Year = Time)
#Renaming "Time" variable as "Year" for clarity

WorldBankData$`GDP per capita in USD` <- as.numeric(WorldBankData$`GDP per capita in USD`) 
WorldBankData$`Exports of goods and services in USD` <- as.numeric(WorldBankData$`Exports of goods and services in USD`)
WorldBankData$`Unemployment Percentage` <- as.numeric(WorldBankData$`Unemployment Percentage`)
WorldBankData$`Country Name` <- as.factor(WorldBankData$`Country Name`)
WorldBankData$`Country Code` <- as.factor(WorldBankData$`Country Code`)
WorldBankData$Year <- as.factor(WorldBankData$Year)
#reclassifing variables as numeric and factor

WorldBankData <- WorldBankData[as.logical((rowSums(is.na(WorldBankData))-3)),]
#removing rows with all 3 drivers missing

WorldBankData <- WorldBankData[-c(9823, 9824, 9825, 9826, 9827), ]
#removing the last 5 rows which don't contain any necessary data
view(WorldBankData)



