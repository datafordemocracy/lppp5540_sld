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
# install.packages("broom", "corrplot", "ggeffects", "vip", "splines") # if you've not used these before
library(tidyverse)
library(broom)
library(corrplot)
library(ggeffects)
library(vip)
library(splines)

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

# distribution: peak size
ggplot(sci, aes(x = peak_size)) + 
  geom_histogram() 

ggplot(sci, aes(x = peak_size)) + 
  geom_density() 

ggplot(sci, aes(x = peak_size, color = acute_protract)) +
  geom_density()

sci %>% 
  mutate(disp_start_bin = cut(disp_start, 
                              breaks = c(1960, 1980, 2000, Inf))) %>% # look! you can pipe into ggplot!
ggplot(aes(x = peak_size, fill = disp_start_bin, alpha = 1/10)) + # some examples of more aesthetics
  geom_density()

# boxplot: duration by conflict type
ggplot(sci, aes(x = conflict_type, y = duration)) + 
  geom_boxplot() 

ggplot(sci, aes(x = fct_lump(conflict_type, n = 3), y = duration)) + # changing the factor on the fly
  geom_boxplot()

ggplot(sci, aes(x = factor(dest_infrastructure), y = duration)) + # alternative to boxplot
  geom_violin()

ggplot(sci, aes(x = factor(bombings), y = duration)) + 
  geom_violin() + geom_jitter(width = 0.1)

# two categorical: a possibility
ggplot(sci, aes(y = conflict_type, x = factor(ethnic_comp))) +
  geom_jitter(aes(color = conflict_type), position=position_jitter(0.2))

# scatterplot: duration and peak size
sci %>% filter(!is.na(duration)) %>% # remove missing duration (it messes with the y scale)
ggplot(aes(x = duration, y = peak_size)) + 
  geom_point() 
# add color = conflict_type, size = gdp_per

# use "censored" duration
ggplot(sci, aes(x = duration_c, y = peak_size, color = ongoing)) + 
  geom_point() 
# factor ongoing, add a smoothed regression

# scatterplot: peak size and population
ggplot(sci, aes(x = pre_pop, y = peak_size)) + 
  geom_point() 

ggplot(sci, aes(x = log(pre_pop), y = log(peak_size), color = conflict_type, size = casualty_rate)) + 
  geom_point() # transform values on the fly


# .........................
# 3. Linear regression ----
# repeated from the webpage

# model 1
lm_peak <- lm(peak_size ~ fragility + pre_pop + pol_terror + polity + gdp_per + hdi, 
              data = sci)
summary(lm_peak)

# effect plot: here the predicted y as x (pol_terror) varies, holding other vars constant
plot(ggpredict(lm_peak, terms = c("pol_terror"))) 
# variable importance plot: for linear models, this is just the t value
vip(lm_peak) 

# stepping back -- correlation matrix of variables (numeric/continuous)
# create a subset of the data used in the model above
sci_tmp <- sci %>% 
  select(peak_size, fragility, pre_pop, pol_terror, polity, gdp_per, hdi) %>% # just the variables from above
  drop_na  # drop missing obs (which are dropped from the model above as well)
  
cor_mat <- cor(sci_tmp) # generate a correlation matrix
cor_mat

# it's easier to visualize (this is where the corrplot package comes in)
corrplot(cor_mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# arguments: show just the upper half of the graph, order the matrix (via hiearchical clustering), make the labels black, and rotate hte upper labels

# or with a heatmap
col <- colorRampPalette(c("blue", "white", "orange"))(100) # make a blue/white/orange color palette
heatmap(x = cor_mat, col = col, symm = TRUE)


# model 2: add factors
lm_peak_fct <- lm(peak_size ~ fragility + pol_terror + polity + gdp_per + hdi + conflict_type, 
              data = sci)
summary(lm_peak_fct)


# model 3: add interactions
lm_peak_int <- lm(peak_size ~ fragility*conflict_type + pol_terror + polity + gdp_per + hdi, 
                  data = sci)
summary(lm_peak_int)

# effect plot
plot(ggpredict(lm_peak_int, terms = c("fragility", "conflict_type"))) 
# variable importance plot
vip(lm_peak_int) 


# model 4: add nonlinearities with polynomials
lm_peak_poly <- lm(peak_size ~ fragility + poly(pol_terror, 2) + polity + gdp_per + hdi, 
                 data = sci)
summary(lm_peak_poly)

# effect plot
plot(ggpredict(lm_peak_poly, terms = c("pol_terror"))) 
# variable importance plot
vip(lm_peak_poly) 


# model 5: add nonlinearities with splines
lm_peak_spl <- lm(peak_size ~ fragility + ns(pol_terror, df = 2) + polity + gdp_per + hdi,
                  data = sci)
summary(lm_peak_spl)

# effect plot
plot(ggpredict(lm_peak_spl, terms = c("pol_terror"))) 
# variable importance plot
vip(lm_peak_spl) 


# model 6: add nonlinearities in y, log
lm_peak_log <- lm(log(peak_size) ~ fragility + pol_terror + polity + gdp_per + hdi,
                  data = sci)

summary(lm_peak_log)
coef(lm_peak_log) %>% exp() %>% round(3)

# effect plot
plot(ggpredict(lm_peak_log, terms = c("pol_terror"))) 
# variable importance plot
vip(lm_peak_log) 


# A few model evaluation possibilities (briefly)
# information criteria (lower is better)
# These are not hypothesis tests. They estimate the out-of-sample accuracy 
AIC(lm_peak, lm_peak_fct, lm_peak_int, lm_peak_poly, lm_peak_spl, lm_peak_log)
BIC(lm_peak, lm_peak_fct, lm_peak_int, lm_peak_poly, lm_peak_spl, lm_peak_log)
# but wait! only models with same y value can be compared...
# among models on non-transformed y, those with nonlinear effects of pol_terror are preferred

# how well does the model generate outcomes like those we observe?
sim_peak <- simulate(lm_peak, nsim = 50) # generate 50 simulated outcome vectors
plot(density(sci$peak_size)) # the density of y (using base R plot)
lines(density(sim_peak$sim_1), col = "grey80") # the outcomes simulated from the model
for(i in 1:50)lines(density(sim_peak[[i]]), lty = 2, col = "grey80") # all 50 simulations

sim_peak_spl <- simulate(lm_peak_spl, nsim = 50)
ggplot(sci, aes(x = peak_size)) + # we'll use ggplot this time
  geom_density() +
  geom_density(aes(sim_1), sim_peak_spl, color = "red")

ggplot(sci, aes(x = peak_size)) + # now with all the simulations
  geom_density() +
  geom_density(aes(x = peak_size, group = sim), 
               pivot_longer(sim_peak_spl, everything(), 
                            names_to = "sim", 
                            values_to = "peak_size"),
               color = "grey80")

sim_peak_log <- simulate(lm_peak_log, nsim = 50)
ggplot(sci, aes(x = log(peak_size))) + # now with all the simulations
  geom_density() +
  geom_density(aes(x = peak_size, group = sim), 
               pivot_longer(sim_peak_log, everything(), 
                            names_to = "sim", 
                            values_to = "peak_size"),
               color = "grey80")


# show coefficient plot (Using tidy from broom package)
tidy_peak <- tidy(lm_peak_log, conf.int = TRUE, conf.level = 0.9)

ggplot(tidy_peak[2:6,], aes(x = estimate, y = term,
                      xmin = conf.low,
                      xmax = conf.high)) + 
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() +
  labs(x = "Coefficient Estimate", y = "")


# .........................
# 4. Save data ----
saveRDS(sci, "sci_week2.RDS")
# sci <- readRDS("sci_week2.RDS")
# Close the project (File -- Close project) before quitting your R session
