# .........................
# Saving Lives with Data II
# Problem Set 1
# Getting Started with R
# Exploring SCI Data
# .........................

# To understand something about the Save the Children International's current model, 
#   we need to understand somethign about the data underlying it. For this problem set, 
#   you'll save this R file with your initials (e.g., probset1_mpc.R), write and execute 
#   the code to display answers to the questions, and then verbally answer the questions 
#   using comments after the code. We should be able to open your scripts, run your code, 
#   and recreate your answers fairly effortlessly. Note, there's always more than one 
#   way to generate the same answer!

# You'll want to refer to the newnames.xlsx file to understand the variable names and 
#   definitions, and to the definitions and data collection tabs of the initial excel
#   file provided by SCI to better understand what the variables represent. The SCI 
#   only recently shared their prototype and data set with us, so it's new to all of 
#   us and we'll be learning more about it together.

# .........................
# 0. Read in the SCI data, load necessary libraries ----
# Use either the rds data frame created during class or the original data frame, 
#   but be sure to repeat the miminal cleaning we did in class 
#   (remove the empty columns, rename the variables).

library(tidyverse)
library(readxl)
sci <- readRDS("sci.rds") 

# Below are multiple ways of getting at most of the answers (though not all of the ways).

# .........................
# 1. General Data Summaries ----

# a. In what year does the earliest conflict-driven displacement event start? 
#   What is the latest displacement onset year?
range(sci$disp_start)
summary(sci$disp_start)
min(sci$disp_start)
max(sci$disp_start)
# earliset: 1961, latest: 2016

# b. On average, what percent of a country's population is diplaced (at the peak) 
#   in the events represented in this data?
summary(sci$peak_size_per)
mean(sci$peak_size_per)
sci %>% summarize(mean(peak_size_per))
# average: .095, or 9.5%

# c. What is the minimum, median, and maximum casualty rate among these 
#   displacement-inducing conflicts?
summary(sci$casualty_rate)
min(sci$casualty_rate, na.rm = TRUE)
median(sci$casualty_rate, na.rm = TRUE)
max(sci$casualty_rate, na.rm = TRUE)
# min: 0, median: 3,120, max: 650,000


# .........................
# 2. Filtering, Selecting, Tables ----

# a. Which 5 countries are experience the most risk of (i) earthquakes, (ii) floods, 
#   (iii) tsunamis, (iv) tropical cyclones, and (v) drought?
sci %>% arrange(earthquake_risk) %>% select(country, earthquake_risk) %>% tail(5) 
# (i) earthquake: Iran, Nepal, Tajikistan, Kyrgyzstan, Guatemala

sci %>% select(country, flood_risk) %>% arrange(desc(flood_risk)) %>% head(5) 
# (ii) Bangladesh, Viet Nam, Iraq, Iraq, Iraq
sci %>% select(country, flood_risk) %>% group_by(country) %>% slice(1) %>% arrange(desc(flood_risk)) %>% head(5) 
# (ii) Bangladesh, Viet Nam, Iraq, Cambodia, Lao People's Dem. Rep.

sci %>% select(country, tsunami_risk) %>% arrange(tsunami_risk) %>% tail(5) 
# (iii) Indonesia, China, Bangladesh, Nicaragua, India

sci %>% select(country, cyclone_risk) %>% arrange(desc(cyclone_risk)) %>% head(5) 
# (iv) China, Viet Nam, Mexico, India, Haiti

sci %>% select(country, drought_risk) %>% arrange(drought_risk) %>% tail(5) 
# (v) Somalia, Zimbabwe, Zimbabwe, Sudan, Sudan
sci %>% select(country, drought_risk) %>% group_by(country) %>% slice(1) %>% arrange(drought_risk) %>% tail(5) 
# (v) Somalia, Zimbabwe, Sudan, South Africa, Namibia


# b. For many of the variables, we see the response "ongoing". How many events are 
#   categorized as ongoing based on displacement end year? Based on displacement
#   peak year? Based on year displacement reaches a minimum? Why do you suppose 
#   these differ? (This really is a thing about the coding here we need to understand 
#   better, as we'll need to recode the "ongoing" responses to format some of 
#   these variables as numeric or factors and want to make sure we're making a 
#   reasonable choice.)
table(sci$disp_end) # 40
sum(sci$peak_year == "ongoing") # 27
sci %>% filter(min_year == "ongoing") %>% count()
length(which(sci$disp_end == "ongoing"))
# I currently assume that fewer events are categorized as ongoing for peak year 
#   because some events, while ongoing, are subsiding. I can't come up with a 
#   justification for more ongoing events based on minimum year

# c. How many displacement events in this dataset fall into each of the categories 
#   of duration? Into each of the categories of size? Does there appear to be a 
#   relation between these two outcomes (note the categories are not yet ordered 
#   properly, we'll fix that once we decide what to do with ongoings...)
table(sci$duration_cat) # <5: 22, 5-20: 26, >20: 22, ongoing: 27
summary(as.factor(sci$size_cat)) # <175: 28, 175-600: 26, >600: 27, ongoing: 16
table(sci$duration_cat, sci$size_cat) %>% prop.table(margin = 2)
# Yes, to some degree, a higher rate of the smallest events fall into the shortest 
#   category (53%); a higher percent of the mid-sized events fall into the
#   mid-length category (42%); a higher percent of the largest events fall 
#   into the longest category (37%) -- the plurality in each case.


# .........................
# 3. Grouping and Summarizing ----

# a. Looking at duration and size again, what is the average peak size of a 
#   displacement event among each category of duration? Do you see any pattern?
sci %>% group_by(duration_cat) %>% summarize(mean(peak_size)) 
# <5: 140,954, 5-20: 462,742, >20: 1,125,989, ongoing: 1,343,192
# yep, mean peak size increases as duration category is longer

# b. What is the mean and standard deviation of (pre) population size by displacement 
#   scale/size categories? Do these appear related?
sci %>% group_by(size_cat) %>% summarize(mean(pre_pop), sd(pre_pop )) 
# <175: 21,726,331 (46,100,307), 175k-600k: 18,030,921 (30,251,345), >600k: 24,476,467 (39,451,978), ongoing: 177,657,256 (367,374,915)
#   less clear -- the average pop size is smaller for the mid-size event than the small or large event 
#   (similarly, the variance is less); the average pop size (and std dev) among ongoing cases appears categorically larger.

# c. Does type of conflict appear related to average displacement scale (peak_size) 
#   or the percent of the population affected?
sci %>% group_by(conflict_type) %>% summarize(mean(peak_size), mean(peak_size_per))
# a trickier question -- is there an order in confict type? -- maybe 
# but average peak size is largest for: multiple conflicts, civil conflicts, 
#   multiple civil conflicts, interstate, and then independence while average pop
#   impacted is largest for: interstate, independence, multiple conflicts, 
#   civil conflict, and then multiple civil conflicts nothing really strikes 
#   me here as systematic.

# .........................
# 4. From the inital spreadsheet provided by SCI, read in the data from the spreadsheet 
#   tabs "Duration" and "Scale". These are the variables currently used for the models
#   predicting each of these outcomes (a subset of the variables in Dataset_master 
#   from above).

# a. How many observations are used to model scale? To model duration?
dur <- read_excel("SC_Data for predictive modeling_vf.xlsx", sheet = "Duration", skip = 3)
scl <- read_excel("SC_Data for predictive modeling_vf.xlsx", sheet = "Scale", skip = 3)
dim(dur)
nrow(scl)
# n = 70 (max) for duration, n = 81 (max) for scale
# look back at 2c
table(sci$duration_cat) # 70 non-ongoing
table(sci$size_cat) # 81 non-ongoing


# b. What are the key concepts that appear to have been used to model duration or 
#    scale -- choose one to focus on. We're not primarily looking for a list of 
#    variable names, but the underlying concepts they represent.
# [Hint: you definitely want to dig into the SCI data collection tab in the initial spreadsheet]
names(dur)
names(scl)
# find these on the data collection tab to understand what represent
# Scale: Conflict type, state power structure (fragility, political terror, polity, 
#   state harmonization), home region attraction (environmental risk, economic risk),
#   size and density (population, area, urbanization), funding of conflict,
#   ease of movement/difficulty of terrain, conflict intensity (casualties, destruction,
#   bombings), external involvement
# Duration: Conflict type, state power structure (political terror, fragility, polity,
#   start harmonization), home region attraction (environmental, economic risk),
#   size and density (populaiton, urbanization), funding of conflict, 
#   ease of movement/difficulty of terrain, conflict intensity (casualties, destruction,
#   bombings), external involvement

# Same concepts/metrics, largely same measures

# c. Identify something about this data that isn't clear from the spreadsheet documentation?

# A key takeaway from the questions is the importance of complete documentation for replicability. 
# Something we will want to take seriously in our work.

