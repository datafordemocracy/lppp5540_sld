library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyverse)
library(tidyr)

WorldBankData <- read_excel("WorldBankData.xlsx")

WorldBankData <- WorldBankData[-4] # remove time code

WorldBankData <- rename(WorldBankData, Year = Time)
view(WorldBankData)

WorldBankData$`GDP per capita in USD` <- as.numeric(WorldBankData$`GDP per capita in USD`) 
WorldBankData$`Exports of goods and services in USD` <- as.numeric(WorldBankData$`Exports of goods and services in USD`)
WorldBankData$`Unemployment Percentage` <- as.numeric(WorldBankData$`Unemployment Percentage`)
WorldBankData$`Country Name` <- as.factor(WorldBankData$`Country Name`)
WorldBankData$`Country Code` <- as.factor(WorldBankData$`Country Code`)
WorldBankData$Year <- as.factor(WorldBankData$Year)

#Read in Gini Data
WBGini <- read_excel("WBGini.xls")

#Code variables in the right format
WBGini$`Country Name` <- as.factor(WBGini$`Country Name`)
WBGini$`Country Code` <- as.factor(WBGini$`Country Code`)

# convert from a wide to a long table
Gini_long <- pivot_longer(WBGini, cols=3:62, names_to = "Year", values_to = "Gini")

#Remove rows with no Gini scores
Gini_Final <- Gini_long %>% drop_na()
view(Gini_Final)

