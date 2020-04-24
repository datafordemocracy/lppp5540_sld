#Loading Libraries
library(dplyr)
library(readr)
library(readxl)
library(tidyverse)

#reading World Bank Data
WBD <- read_excel("wbd/WBD.xlsx")

#Remove Time Code Column
WBD <- WBD[-4] 

#Adding in New Titles for clarity
newtitles <- read_excel("wbd/newtitles.xlsx") 
names(WBD) <- newtitles$data

#Reclassifing Variables
WBD$`name` <- as.factor(WBD$`name`)
WBD$`iso` <- as.factor(WBD$`iso`)
WBD$year <- as.numeric(WBD$year)
WBD$`aid` <- as.numeric(WBD$`aid`)
WBD$`inf_mor` <- as.numeric(WBD$`inf_mor`)
WBD$`und_nor` <- as.numeric(WBD$`und_nor`)
WBD$`inc_per` <- as.numeric(WBD$`inc_per`) 
WBD$`unemp` <- as.numeric(WBD$`unemp`) 
WBD$`net_exp` <- as.numeric(WBD$`net_exp`) 
WBD$`gdp_pc` <- as.numeric(WBD$`gdp_pc`)
WBD$`gdp` <- as.numeric(WBD$`gdp`)

#Removing the last 5 rows of blank data
WBD <- WBD[-c(13021, 13022, 13023, 13024, 13025), ]

#Removing all years 2019
WBD <- subset(WBD, year != 2019)

#Removes all years earlier than 2000
WBD <- subset(WBD, year >= 2000)
df <- subset(df, year >= 2000)
missing_values <- WBD %>%
  gather(driver, num_missing) %>%
  mutate(is.missing = is.na(num_missing)) %>%
  group_by(driver, is.missing) %>%
  summarise(num_missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num_missing)) 

missing_values2 <- df %>%
  gather(driver, num_missing) %>%
  mutate(is.missing = is.na(num_missing)) %>%
  group_by(driver, is.missing) %>%
  summarise(num_missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num_missing))
#(Adapted From: https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html)
#This shows us how many of each variable is missing.  THere are ~4100 entries in total, so I think the lower four are fine for imputation.
#Drivers that are missing at least 1000 values probably should be dropped or substituted for something else.

load("displacement.rdata")
library(VIM)
library(ggplot2)
library(tidyverse)
library(broom)
library(corrplot)
library(splines)
library(ggeffects)
library(vip)

#For all the below plots, I had to disable all other packages for it to work.  I'm not sure which single one caused the conflict.
library(ggplot2)

ggplot(df, aes(x = gdp_pc, y = size)) +
  geom_point()

ggplot(df, aes(x = gdp.y, y = size)) +
  geom_point()

ggplot(df, aes(x = inf_mor, y = size)) +
  geom_point()

ggplot(df, aes(x = net_exp, y = size)) +
  geom_point()

install.packages("mice")
library("mice")

tempData <- mice(WBD,m=5,maxit=50,seed=500)
summary(tempData)

WBD <- as.data.frame(WBD)
write_csv(WBD, path = "wbd/WBD.csv")

