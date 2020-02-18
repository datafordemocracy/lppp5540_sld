# .........................
# Saving Lives with Data II
# 2020-02-14, MPC
# Getting Started with R
# Exploring SCI Data
# .........................
# 1. Orientation to R/Rstudio (scripts, packages, shortcuts, assignment)
# 2. Reading/viewing data (working directory/Rprojects, reading, summarizing, variable types/factors)
# 3. Exploring data/dplyr (pipes, filter, group_by/summarize)
# 4. Resources


# .........................
# 1. Orientation to R/Rstudio ----

# Script editor (like your do-file editor in Stata): write/save reusable code
#   To create: File... New File... R Script; script icon; Cmd/Ctrl+Shift+N
# Console (like command window and results in Stata): write and execute commands, see results from executed commands
# Global Environment (like variables and review in Stata): Environment - list all objects in memory, 
#   History - list recently executed commands, Connections - connect to external data soures (e.g., databases)
# Files, etc: Files -  in current working directory, Plots - where grpahical output appears, Packages - shows installed/loaded packages, 
#   Help - documentation for R packages/functions, Viewer - view locally stored web content (e.g., html files from Rmarkdown, Shiny apps)
#   ?functionname will pull up the function help files; the examples at the end of most help files are especially useful

# R scripts (saved as .R files) are your friend -- this is one.
#   R scripts should have lots of comments (like these).

# Base R (what you open initially) comes with about 30 packages containing functions that operate on data; 
#   there are over 10K user-contributed packages discoverable on CRAN (the Comprehensive R Archive Network): https://cran.r-project.org/
#   To install packages (done once): Tools... Install Packages... Enter package name... Install; or run the command - install.packages("packagename")
#   To load packages (done every new session): run the command - library(packagename)

# Shortcuts: RStudio has created many keystroke shortcuts (you can even create your own). You can find a full list here: https://rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf
#   Run a command (with your cursor anywhere in that code chunk): Cmd/Ctrl+Enter
#   Comment/Uncomment: Cmd/Ctrl_Shift+C
#   Clear console: Cmd/Ctrl+L

# install.packages("tidyverse") # if you haven't already installed tidyverse, run this first; if you have installed it, skip this command
library(tidyverse)

# We'll need this, too
install.packages("readxl")
library(readxl)

# R can perform operations on the fly
10*2.5 # the maximum number of hours we have together

# To save anything in session memory, you need to create an object using the assignment operator: <-
#   objectname <- value
#   Shortcut for assignment operator: Option/Alt+-
maxhrs <- 10*2.5 # now it's in your environment!


# .........................
# 2. Reading/viewing data ----

# What's your current working directory (where R will look for stuff)?
getwd() # or just look at the file path in Files

# Change your working directory: setwd("path")
#   type setwd("") and, with your cursor in the quotes, hit tab
setwd("")
setwd("~/") # or start from your default home directory
#   this is an example of the autocomplete function in Rstudio - userful for directories, variable names, and more!
#   you can also go to Session... Set Working Directory... Choose Directory or on the File tab click More... Set As Working Directory

# But Rprojects are easier, more reproducible and shareable!
#   Create an R project to keep all the files associated with a given project together: File... New Project... Existing Directory (probably)
#   When you open an Rproject file, a new R session will start with the working directory set to where you created the Rproject, the history, and more
#   Plus, collaborators can use the same script in their own directories (even though these will be different)


# Read in the SCI data, provided in an excel file 
# objectname <- read_excel("filename.xlsx")
# ?read_excel # check out the read_excel help file

sci <- read_excel("SC_Data for predictive modeling_vf.xlsx", sheet = "Dataset_master", skip = 3)

# There many other read commands for differnet format types
read # hit tab with your cursor at the end of this command

# Explore
names(sci) # variable names (or click on the arrow by the object in the environment)
#   many of these are not valid R names (e.g., contain spaces) -- we'll want to change these
View(sci) # spreadsheet view (or click on the object in the environment)
head(sci) # print first 6 records to console
?head # check ou the head help file; help has two arguments named x and n
head(sci, n = 10) # change the second argument
head(sci, 10) # if the value of arguments are in the same order as they appear in the function, you don't have to write the argument name
tail(n=5, x = sci) # if the value of arguments aren't in the same order, you must identify the name of the argument


# Object classes
class(sci) # R traditionally uses data frames -- this is one -- but the tidyverse uses tibbles, "an opinionated data frame" (readxl is part of the tidyverse)
#   everyting in R is an object (R is an object oriented programming language); objects have defined structures and methods
#   other object classes include: vector, matrix, list

# Variable types
str(sci) # the structure of the variables (or click on the arrow by the object in the environment)
#   main types are: numeric, integer, logical, character, factor 
#   read_excel makes as few assumptions as possible about the appropriate variable type, so many of these are not wnat we would ultimately want
#   in particular, many are categorical -- in R we'll call them factors (variables stored as integers - 1,2,3 - but with associated labels)


# Summary, summary measures
summary(sci)
#   summary doesn't operate on characters
summary(sci$`Pre-DP start year`) # $ is a way of indexing data frames, referencing a variable of interest

# or each measure individually
mean(sci$`Peak size`)
median(sci$`Peak size`)
sd(sci$`Peak size`)
max(sci$`Peak size`)
min(sci$`Peak size`)
range(sci$`Peak size`)
#   these are often most useful within other functions, in deriving new variables, in evaluating erors and such


# .........................
# 3. Exploring data/dplyr ----

# dplyr, a core part of the tidyverse, is a package for data manipulation. It implements a grammar for transforming data, 
#   based on verbs/functions that define a set of common tasks.
#   In general, the first argument of a dplyr command is the data frame name; and dplyr functions return data frames (tibbles)


# Isolating data: select - extract columns by name
select(sci, Country)
select(sci, Country, HDI) # can list multiple elements
select(sci, Country:`Conflict description`) # or a range

# Before we move on, get rid of empty colums: to select everything BUT a few columns, use the negation, - , inside a combine function, c().
sci <- select(sci, -c(`0...26`, `0...38`, `0...46`, `0...51`, `0...56`))

# and while we're at it, let's rename the variables using simpler (valid) names
newnames <- read_excel("newnames.xlsx")  
names(sci) <- newnames$newname


# Isolating data: filter - extract rows by logical conditions
filter(sci, disp_end == "ongoing") # which displacements are ongoing?
filter(sci, disp_end == "ongoing" & conflict_end != "ongoing") # ongoing displacements where initiating conflict is not ongoing
filter(sci, disp_end == "ongoing" & disp_start %in% 2010:2016) # the %in% operator os often very useful


# The pipe operator
# dplyr (and the tidyverse more generally) make frequent use of pipe operator, %>%. It passes the result on the LHS into the first argument of the function on the RHS.
# We can use it string together multiple manipulations without having to save intermediate results
# Keystroke shortcut: Cmd/Ctrl+Shift+M

# For example, how long have the ongoing displacements been ongoing?
sci %>% 
  filter(disp_end == "ongoing") %>% 
  select(country, disp_start) %>% # reduce to show just key columns
  arrange(disp_start) # arrange is another dplyr verb


# Deriving data: summarize - summarize variables, compute tables of summaries
sci %>% 
  filter(disp_end == "ongoing") %>% 
  summarize(oldest = min(disp_start), # return the smallest value of year
            newest = max(disp_start), # return the largest value of year
            total = n()) # return the number of values in df (e.g., after filtering)
#   useful summary functions: min(), max(), n(), mean(), sd(), median(), first(), last()


# Deriving data: group_by -- group rows
# the combination of group_by and summarize can be pretty powerful
sci %>% filter(disp_end == "ongoing") %>% 
  group_by(conflict_type) %>% # same summaries but separately by conflict type
  summarize(oldest = min(disp_start), 
            newest = max(disp_start), 
            total = n())

sci %>% filter(disp_end == "ongoing") %>% 
  count(size_cat) # count() is short hand for group_by(var) %>% summarize(n()), and a useful way to get a table of frequencies


# But this an easier way to get frequency tables and cross-tabs (base R)
table(sci$disp_cat) # frequency table, cases in each time period
table(sci$disp_cat, sci$size_cat) # cross-tabluation
prop.table(table(sci$disp_cat, sci$size_cat)) # as cell proportions
prop.table(table(sci$disp_cat, sci$size_cat), margin = 1) # as row proportions
prop.table(table(sci$disp_cat, sci$size_cat), margin = 2) # as column proportions

# You can still use the pipe! (sometimes)
table(sci$disp_cat) %>% prop.table()


# .........................
# 4. Save stuff ----

# save just the sci data frame for later use
saveRDS(sci, file = "sci.rds") # will put this in your current working directory unless a path is provided
# sci <- readRDS("sci.rds") # to read it in later

# save a csv file of the sci data (for easy interoperability)
write_csv(sci, path = "sci.csv") 
# sci <- read_csv("sci.csv")

# save everything in memory 
save.image("sci_day1.Rdata") 
# load("sci_day1.Rdata")


# .........................
# 5. Helpful Online Resources ----

# PhD Plus series materials: https://uvastatlab.github.io/phdplus/
# Spring 2020 Library RDS workshop materials: https://data.library.virginia.edu/training/
# dplyr cheat sheet: https://www.rstudio.com/resources/cheatsheets/#dplyr 
# Grolemund and Wickhams's R for Data Science: https://r4ds.had.co.nz/
# Irizarry's Introduction to Data Science: https://rafalab.github.io/dsbook/
# Online searches (google, stackoverflow)
