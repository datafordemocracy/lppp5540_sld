# .........................
# Saving Lives with Data II
# Problem Set 1
# Getting Started with R
# Exploring SCI Data
# .........................

# To understand something about the Save the Children International's current model, we need to understand somethign about the data underlying it.
# For this problem set, you'll save this R file with your initials (e.g., probset1_mpc.R), write and execute the code to display answers to the questions,
# and then verbally answer the questions using comments after the code. We should be able to open your scripts, run your code, and recreate your
# answers fairly effortlessly. Note, there's always more than one way to generate the same answer!

# You'll want to refer to the newnames.xlsx file to understand the variable names and definitions, and to the definitions and data collection tabs
# of the initial excel file provided by SCI to better understand what the variables represent. The SCI only recently shared their prototype and 
# data set with us, so it's new to all of us and we'll be learning more about it together.


# .........................
# 0. Read in the SCI data, load necessary libraries
#  Use either the rds data frame created during class or the original data frame, but be sure to repeat the miminal cleaning we did in class
# (remove the empty columns, rename the variables).



# .........................
# 1. General Data Summaries

# a. In what year does the earliest conflict-driven displacement event start? What is the latest displacement onset year?

# b. On average, what percent of a country's population is diplaced (at the peak) in the events represented in this data?

# c. What is the minimum, median, and maximum casualty rate among these displacement-inducing conflicts?



# .........................
# 2. Filtering, Selecting, Tables

# a. Which 5 countries are experience the most risk of (i) earthquakes, (ii) floods, (iii) tsunamis, (iv) tropical cyclones, and (v) drought?

# b. For many of the variables, we see the response "ongoing". How many events are categorized as ongoing based on displacement end year? 
#   Based on displacement peak year? Based on year displacement reaches a minimum? Why do you suppose these differ? 
#   (This really is a thing about the coding here we need to understand better, as we'll need to recode the "ongoing" responses to
#   format some of these variables as numeric or factors and want to make sure we're making a reasonable choice.)

# c. How many displacement events in this dataset fall into each of the categories of duration? Into each of the categories of size? 
#   Does there appear to be a relation between these two outcomes (note the categories are not yet ordered properly, we'll fix that 
#   once we decide what to do with ongoings...)



# .........................
# 3. Grouping and Summarizing

# a. Looking at duration and size again, what is the average peak size of a displacement event among each category of duration? Do you see any pattern?

# b. What is the mean and standard deviation of (pre) population size by displacement scale/size categories? Do these appear related?

# c. Does type of conflict appear related to average displacement scale (peak_size) or the percent of the population affected?



# .........................
# 4. From the inital spreadsheet provided by SCI, read in the data from the spreadsheet tabs "Duration" and "Scale". These are the variables currently 
# used for the models predicting each of these outcomes (a subset of the variables in Dataset_master from above).

# a. How many observations are used to model scale? To model duration?

# b. What are the key concepts that appear to have been used to model duration or scale -- choose one to focus on. We're not primarily looking
# for a list of variable names, but the underlying concepts they represent.
#   [Hint: you definitely want to dig into the SCI data collection tab in the initial spreadsheet]

# c. Identify something about this data that isn't clear from the spreadsheet documentation?
