# .........................
# Saving Lives with Data II
# 2020-02-21, MPC
# Wrangling and Regression in R
# Exploring SCI Data
# .........................
# 0. Set up (packages, data)
# 1. More dplyr (mutate, factors)
# 2. A little ggplot
# 3. Linear regression
# 4. Save stuff


# .........................
# 0. Set up ----

# libraries
library(tidyverse)
# install.packages("broom", "corrplot") # if you've not used these before
library(broom)
library(corrplot)

# data
sci <- readRDS("sci.rds") 


# .........................
# 1. More data wrangling/dplyr ----
# Mutate and factors

# An example
str(sci$acute_protract) 

# Factors are variables which take on a limited number of values, aka categorical variables. 
# In R, factors are stored as a vector of integer values with the corresponding label (in R, it's called a "level) 

sci %>% mutate(acute_protract = factor(acute_protract)) %>% count(acute_protract)

# assert the order of the factor levels (this time, saving it in the data frame)
sci <- sci %>% mutate(acute_protract = factor(acute_protract, levels = c("acute", "protracted", "ongoing")))
table(sci$acute_protract)

# let's check out the 13 ongoing cases
sci %>% filter(acute_protract == "ongoing") %>% select(country, disp_start)
# so ongoing is just capturing new (at the time) events

# Another example
# conflict_type
table(sci$conflict_type) 
con_level <- c("Independence", "Civil conflict", "Multiple civil conflicts", "Interstate", "Multiple conflicts")
sci %>% mutate(conflict_type = factor(conflict_type, levels = con_level)) %>% count(conflict_type)

# or sequence the levels by something else (frequency)
sci %>% mutate(conflict_type = fct_infreq(conflict_type)) %>% count(conflict_type)
# or combine categories - not clear that makes sense here (lump into the most frequent n values plus everything else)
sci %>% mutate(conflict_type = fct_lump(conflict_type, n = 3)) %>% count(conflict_type)

sci <- sci %>% mutate(conflict_type = fct_infreq(conflict_type)) %>% filter(TRUE)

# What about all of those ongoing? For now, I'm going to change ongoing to missing (we can choose something else later)
# (essentially what BCG does)
sci <- sci %>% mutate(disp_end = as.numeric(disp_end))

# I can apply this function to multiple variables at the same time
vars <- c("peak_year", "duration") # create a vector of variable names
sci <- sci %>% mutate_at(vars, as.numeric) # mutate_at

# or, I could make a *new* censored version of a variable
sci <- sci %>% mutate(duration_c = if_else(is.na(duration), 2016-disp_start, duration))


# .........................
# 2. A little ggplot ----

# the "grammar of graphics"
# basic syntax: 
# ggplot(data, aes(x = , y = , color = , shape = , size = )) + 
#   geom_point() [or] geom_line() [or] geom_histogram() [or] geom_boxplot() [etc.]
# maps from data to aes/aesthetic attributes (location, color, etc.) of geom/geometric objects (points, lines, etc.)
# more on scales and facets next week...

ggplot(sci, aes(x = peak_size)) + 
  geom_histogram() # distribution of peak size

ggplot(sci, aes(x = conflict_type, y = duration)) + 
  geom_boxplot() # duration by conflict type

sci %>% filter(!is.na(duration)) %>% # look! you can pipe into ggplot!
ggplot(aes(x = duration, y = peak_size)) + 
  geom_point() 
# add color = conflict_type, size = gdp_per

ggplot(sci, aes(x = duration_c, y = peak_size, color = ongoing)) + 
  geom_point() 
# factor ongoing, add a smoothed regression

ggplot(sci, aes(x = log(pre_pop), y = log(peak_size), color = conflict_type, size = casualty_rate)) + 
  geom_point() # transform values on the fly


# .........................
# 3. Linear regression ----

# repeated from the webpage
lm_peak <- lm(peak_size ~ fragility + pre_pop + pol_terror + polity + gdp_per + hdi, 
              data = sci)
summary(lm_peak)

### 
# this wasn't on the webpage, though!
# create a subset of the data used in the model above
sci_tmp <- sci %>% 
  select(peak_size, fragility, pre_pop, pol_terror, polity, gdp_per, hdi) %>% # just the variables from above
  drop_na  # drop missing obs (which are dropped from the model above as well)
  
cor_mat <- cor(sci_tmp) # generate a correlation matrix
cor_mat

# this is where the corrplot package comes in
corrplot(cor_mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# says show just the upper half of the graph, order the matrix (via hiearchical clustering), make the labels black, and rotate hte upper labels

# or if you like a good heatmap
col <- colorRampPalette(c("blue", "white", "orange"))(100) # make a blue/white/orange color palette
heatmap(x = cor_mat, col = col, symm = TRUE)
###

# repeated from webpage
# add factors
lm_peak <- lm(peak_size ~ fragility + pol_terror + polity + gdp_per + hdi + conflict_type, 
              data = sci)
summary(lm_peak)

# add interactions
lm_peak_int <- lm(peak_size ~ fragility*conflict_type + pol_terror + polity + gdp_per + hdi, 
                  data = sci)
summary(lm_peak_int)

# add nonlinearities: polynomials
lm_peak_poly <- lm(peak_size ~ fragility + poly(pol_terror, 2, raw = TRUE) + polity + gdp_per + hdi, 
                 data = sci)
summary(lm_peak_poly)

# add nonlinearities: log
lm_peak_log <- lm(log(peak_size) ~ fragility + pol_terror + polity + gdp_per + hdi,
                  data = sci)

summary(lm_peak_log)

# show coefficient plot
tidy_peak <- tidy(lm_peak, conf.int = TRUE, conf.level = 0.9)
ggplot(tidy_peak, aes(x = estimate, y = term,
                      xmin = conf.low,
                      xmax = conf.high)) + 
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() +
  labs(x = "Coefficient Estimate", y = "Intercept and Factors")


# .........................
# 4. Save stuff ----
saveRDS(sci, "sci_week2.RDS")

# Close the project (File -- Close project) before quitting your R session
