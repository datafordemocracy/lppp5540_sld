#######################################
# Saving Lives with Data 2
# Spring 2020
# Michele Claibourn

# 1. Univariate time series imputation
# 2. Implement univariate imputation for all countries
# 3. Implement univariate imputation for 54 countries, 19 year version
# 4. Final example, lopping off the first or last few years
#######################################

library(tidyverse)
library(imputeTS)

load("displacement.rdata")


#......................................
# 1. Univariate time series imputation ----
# use a single country as example: Eritrea
disp_eri <- disp_full %>% filter(iso == "ERI") %>% arrange(year)

# Visualize the missing values of displacement size in this series
plotNA.distribution(disp_eri$size)

# Create some missing values
ind <- sample_frac(disp_eri, .25)
disp_eri <- disp_eri %>% 
  mutate(size_na = if_else(year %in% ind$year, NA_real_, size))

# Visualize the distribution of missing values 
plotNA.distribution(disp_eri$size_na)

# Impute values: mean
disp_eri$size_mean <- na_mean(disp_eri$size_na)

# Visualize imputated values
plotNA.imputations(disp_eri$size_na, disp_eri$size_mean)
# never do this!
# if we have ground truth, as we do here but never really do in practice, we can add that to the plot
plotNA.imputations(disp_eri$size_na, disp_eri$size_mean, disp_eri$size)

# Impute values: locf
disp_eri$size_locf <- na_locf(disp_eri$size_na)

# Visualize imputed values
plotNA.imputations(disp_eri$size_na, disp_eri$size_locf)

# Impute values: moving average
disp_eri$size_ma <- na_ma(disp_eri$size_na, k = 4)

# Visualize imputed values
plotNA.imputations(disp_eri$size_na, disp_eri$size_ma)

# Impute values: linear interpolation
disp_eri$size_lin <- na_interpolation(disp_eri$size_na, option = "linear")

# Visualize imputed values
plotNA.imputations(disp_eri$size_na, disp_eri$size_lin)

# Impute values: spline interpolation
disp_eri$size_spl <- na_interpolation(disp_eri$size_na, option = "spline")

# Visualize imputed values
plotNA.imputations(disp_eri$size_na, disp_eri$size_spl)

# How'd they do? Generate MAPE
disp_eri <- disp_eri %>% 
  mutate(ape_mean = if_else(is.na(size_na), abs(size_mean - size)/size, NA_real_),
         ape_locf = if_else(is.na(size_na), abs(size_locf - size)/size, NA_real_),
         ape_ma = if_else(is.na(size_na), abs(size_ma - size)/size, NA_real_),
         ape_lin = if_else(is.na(size_na), abs(size_lin - size)/size, NA_real_),
         ape_spl = if_else(is.na(size_na), abs(size_spl - size)/size, NA_real_))

disp_eri %>% 
  filter(is.na(size_na) & size != 0) %>% 
  summarize(mean(ape_mean),
            mean(ape_locf),
            mean(ape_ma),
            mean(ape_lin),
            mean(ape_spl))

# clean up
rm(disp_eri, ind)


#......................................
# 2. Implement univariate imputation for all countries ----
#    so each countries series relies only on itself...

# Randomly delete values of size in disp_full
ind_full <- sample_frac(disp_full, .25) %>% 
  mutate(drop = 1) %>% 
  select(iso, year, drop)

disp_full_na <- disp_full %>% 
  left_join(ind_full, by = c("iso", "year")) %>% 
  mutate(size_na = if_else(!is.na(drop), NA_real_, size))

# Plot distribution of missing for a few countries
plotNA.distribution(filter(disp_full_na, iso == "AFG")$size_na)
plotNA.distribution(filter(disp_full_na, iso == "ERI")$size_na)
plotNA.distribution(filter(disp_full_na, iso == "SOM")$size_na)

# Extract distinct values of iso as variable to map over
iso <- disp_full_na %>% distinct(iso) %>% pull(iso) %>% as.character

# Apply imputation function over iso
disp_na <- map_df(iso,
                  function(.x) {
                    disp_full_na %>% 
                      filter(iso == .x) %>% 
                      arrange(year) %>% 
                      mutate(size_locf = na_locf(size_na),
                             size_lin = na_interpolation(size_na, option = "linear"))
                  })

# Plot imputed values for a few countries
# locf
plotNA.imputations(filter(disp_na, iso == "AFG")$size_na, 
                   filter(disp_na, iso == "AFG")$size_locf)

# But here we can add the "ground truth", original values
plotNA.imputations(filter(disp_na, iso == "AFG")$size_na, 
                   filter(disp_na, iso == "AFG")$size_locf,
                   filter(disp_na, iso == "AFG")$size)

# linear
plotNA.imputations(filter(disp_na, iso == "AFG")$size_na, 
                   filter(disp_na, iso == "AFG")$size_lin)

# with original values
plotNA.imputations(filter(disp_na, iso == "AFG")$size_na, 
                   filter(disp_na, iso == "AFG")$size_lin,
                   filter(disp_na, iso == "AFG")$size)


# Generate APE
disp_na <- disp_na %>% 
  mutate(ape_locf = if_else(is.na(size_na), abs(size_locf - size)/size, NA_real_),
         ape_lin = if_else(is.na(size_na), abs(size_lin - size)/size, NA_real_))

# Calculate MAPE
disp_na %>% 
  filter(is.na(size_na) & size != 0) %>% 
  summarize(mean(ape_locf),
            mean(ape_lin))

# Evaluate by country 
disp_mape <- disp_na %>% 
  filter(is.na(size_na) & size != 0) %>% 
  group_by(iso) %>% 
  summarize(mape_locf = mean(ape_locf),
            mape_lin = mean(ape_lin))

summary(disp_mape)

disp_mape %>% arrange(desc(mape_locf)) %>% head(10)
disp_mape %>% arrange(desc(mape_lin)) %>% head(10)


#......................................
# 3. Implement univariate imputation for 54 countries, 19 year version ----
#    some of the places where locf performed better were where missingness occurred between
#    a big change -- if the missing ob was the year a big change occurred, linear performed better;
#    if the missing ob was the year before a big change, locf performed better.
#    With the 2000-2018 data, we should see fewer big changes, so let's try it one more time on 
#    the recent subset of data.
#    UPDATE: where the imputation performed especially poorly among the remaining data
#    tended to be small countries that aren't present in the conflict-experiencing 
#    countries that comprise the data we're primarily working with. So this is updated
#    to include only the smaller subset of countries in only the 19 year period.

disp_2000_na <- disp_full_na %>% filter(year > 1999 & iso %in% disp_conflict$iso)

# Plot distribution of missing for a few countries
plotNA.distribution(filter(disp_2000_na, iso == "AFG")$size_na)
plotNA.distribution(filter(disp_2000_na, iso == "ERI")$size_na)
plotNA.distribution(filter(disp_2000_na, iso == "SOM")$size_na)

# Extract distinct values of iso as variable to map over
iso <- disp_2000_na %>% distinct(iso) %>% pull(iso) %>% as.character

# Apply imputation function over iso
disp_2000 <- map_df(iso,
                  function(.x) {
                    disp_2000_na %>% 
                      filter(iso == .x) %>% 
                      arrange(year) %>% 
                      mutate(size_locf = na_locf(size_na),
                             size_lin = na_interpolation(size_na, option = "linear"))
                  })

# Plot imputed values for a few countries
# locf
plotNA.imputations(filter(disp_2000, iso == "SOM")$size_na, 
                   filter(disp_2000, iso == "SOM")$size_locf)

# But here we can add the "ground truth", original values
plotNA.imputations(filter(disp_2000, iso == "SOM")$size_na, 
                   filter(disp_2000, iso == "SOM")$size_locf,
                   filter(disp_2000, iso == "SOM")$size)

# linear
plotNA.imputations(filter(disp_2000, iso == "SOM")$size_na, 
                   filter(disp_2000, iso == "SOM")$size_lin)

# with original values
plotNA.imputations(filter(disp_2000, iso == "SOM")$size_na, 
                   filter(disp_2000, iso == "SOM")$size_lin,
                   filter(disp_2000, iso == "SOM")$size)

# Generate APE
disp_2000 <- disp_2000 %>% 
  mutate(ape_locf = if_else(is.na(size_na), abs(size_locf - size)/size, NA_real_),
         ape_lin = if_else(is.na(size_na), abs(size_lin - size)/size, NA_real_))

# Calculate MAPE
disp_2000 %>% 
  filter(is.na(size_na) & size != 0) %>% 
  summarize(mean(ape_locf),
            mean(ape_lin))

# Evaluate by country 
disp_mape_2000 <- disp_2000 %>% 
  filter(is.na(size_na) & size != 0) %>% 
  group_by(iso) %>% 
  summarize(mape_locf = mean(ape_locf),
            mape_lin = mean(ape_lin))

summary(disp_mape_2000)
# UPDATE: this is more in line with my expectations, where the linear interpolation
#   performs a little better; but it's worth recognizing that the better approach,
#   according to the MAPE depends on the nature of the series.

disp_mape_2000 %>% arrange(desc(mape_locf)) %>% head(10)
disp_mape_2000 %>% arrange(desc(mape_lin)) %>% head(10)


#......................................
# 4. Final example, lopping off the first or last few years ----

# Create non-random missing values (first three or last three for every country)
disp_2000_na <- disp_full_na %>% filter(year > 1999 & iso %in% disp_conflict$iso)
iso <- disp_2000_na %>% distinct(iso) %>% pull(iso) %>% as.character

set.seed(1017)
endpoint <- sample_frac(as.data.frame(iso), .5) %>% 
  mutate(end = 1,
         iso = as.character(iso)) 

disp_ends_na <- disp_2000_na %>% 
  left_join(endpoint, by = "iso") %>% 
  mutate(size_na = case_when(end == 1 & year > 2015 ~ NA_real_,
                             end == 1 & year <= 2015 ~ size,
                             year < 2003 ~ NA_real_,
                             TRUE ~ size))

# Plot distribution of missing for a few countries
plotNA.distribution(filter(disp_ends_na, iso == "SOM")$size_na)
plotNA.distribution(filter(disp_ends_na, iso == "COL")$size_na)

# Apply imputation function over iso
disp_ends <- map_df(iso,
                    function(.x) {
                      disp_ends_na %>% 
                        filter(iso == .x) %>% 
                        arrange(year) %>% 
                        mutate(size_locf = na_locf(size_na),
                               size_ma = na_ma(size_na, k = 4),
                               size_lin = na_interpolation(size_na, option = "linear"),
                               size_spl = na_interpolation(size_na, option = "spline"),
                               size_kal_auto = na_kalman(size_na, model = "auto.arima"), # ARIMA model forecast
                               size_kal_str = na_kalman(size_na, model = "StructTS")) # structural model via mle
                    })

# Plot imputed values for a few countries as examples (these may not be representative of patterns)
# locf
plotNA.imputations(filter(disp_ends, iso == "SOM")$size_na, 
                   filter(disp_ends, iso == "SOM")$size_locf,
                   filter(disp_ends, iso == "SOM")$size)

plotNA.imputations(filter(disp_ends, iso == "COL")$size_na, 
                   filter(disp_ends, iso == "COL")$size_locf,
                   filter(disp_ends, iso == "COL")$size)

# ma
plotNA.imputations(filter(disp_ends, iso == "SOM")$size_na, 
                   filter(disp_ends, iso == "SOM")$size_ma,
                   filter(disp_ends, iso == "SOM")$size)

plotNA.imputations(filter(disp_ends, iso == "COL")$size_na, 
                   filter(disp_ends, iso == "COL")$size_ma,
                   filter(disp_ends, iso == "COL")$size)

# lin
plotNA.imputations(filter(disp_ends, iso == "SOM")$size_na, 
                   filter(disp_ends, iso == "SOM")$size_lin,
                   filter(disp_ends, iso == "SOM")$size)

plotNA.imputations(filter(disp_ends, iso == "COL")$size_na, 
                   filter(disp_ends, iso == "COL")$size_lin,
                   filter(disp_ends, iso == "COL")$size)

# spl
plotNA.imputations(filter(disp_ends, iso == "SOM")$size_na, 
                   filter(disp_ends, iso == "SOM")$size_spl,
                   filter(disp_ends, iso == "SOM")$size)

plotNA.imputations(filter(disp_ends, iso == "COL")$size_na, 
                   filter(disp_ends, iso == "COL")$size_spl,
                   filter(disp_ends, iso == "COL")$size)

# kal_auto
plotNA.imputations(filter(disp_ends, iso == "SOM")$size_na, 
                   filter(disp_ends, iso == "SOM")$size_kal_auto,
                   filter(disp_ends, iso == "SOM")$size)

plotNA.imputations(filter(disp_ends, iso == "COL")$size_na, 
                   filter(disp_ends, iso == "COL")$size_kal_auto,
                   filter(disp_ends, iso == "COL")$size)

# kal_str
plotNA.imputations(filter(disp_ends, iso == "SOM")$size_na, 
                   filter(disp_ends, iso == "SOM")$size_kal_str,
                   filter(disp_ends, iso == "SOM")$size)

plotNA.imputations(filter(disp_ends, iso == "COL")$size_na, 
                   filter(disp_ends, iso == "COL")$size_kal_str,
                   filter(disp_ends, iso == "COL")$size)

# MAPEs, one more time
# Generate APE
disp_ends <- disp_ends %>% 
  mutate(ape_locf = if_else(is.na(size_na), abs(size_locf - size)/size, NA_real_),
         ape_ma = if_else(is.na(size_na), abs(size_ma - size)/size, NA_real_),
         ape_lin = if_else(is.na(size_na), abs(size_lin - size)/size, NA_real_),
         ape_spl = if_else(is.na(size_na), abs(size_spl - size)/size, NA_real_),
         ape_kal_auto = if_else(is.na(size_na), abs(size_kal_auto - size)/size, NA_real_),
         ape_kal_str = if_else(is.na(size_na), abs(size_kal_str - size)/size, NA_real_))

# Calculate MAPE
disp_ends %>% 
  filter(is.na(size_na) & size != 0) %>% 
  summarize(mean(ape_locf),
            mean(ape_ma),
            mean(ape_lin),
            mean(ape_spl),
            mean(ape_kal_auto),
            mean(ape_kal_str))
# locf and lin are still marginally better

# Evaluate by country 
disp_mape_ends <- disp_ends %>% 
  filter(is.na(size_na) & size != 0) %>% 
  group_by(iso) %>% 
  summarize(mape_locf = mean(ape_locf),
            mape_ma = mean(ape_ma),
            mape_lin = mean(ape_lin),
            mape_spl = mean(ape_spl),
            mape_kal_auto = mean(ape_kal_auto),
            mape_kal_str = mean(ape_kal_str))

summary(disp_mape_ends)

# where does these perform least well?
disp_mape_ends %>% arrange(desc(mape_locf)) %>% head(10)
disp_mape_ends %>% arrange(desc(mape_ma)) %>% head(10)
disp_mape_ends %>% arrange(desc(mape_lin)) %>% head(10)
disp_mape_ends %>% arrange(desc(mape_spl)) %>% head(10)
disp_mape_ends %>% arrange(desc(mape_kal_auto)) %>% head(10)
disp_mape_ends %>% arrange(desc(mape_kal_str)) %>% head(10)
# for consistely worst cases, both exhibit steep rise in missing gap...
# while we can estimate ranges at the end points, 
# it will only estimate reasonably if the series is reasonably stable
