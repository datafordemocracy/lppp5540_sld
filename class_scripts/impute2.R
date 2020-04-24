#######################################
# Saving Lives with Data 2
# Spring 2020
# Michele Claibourn

# 1. View example data
# 2. Basic use
# 3. Incorporating time effects
# 4. Transforming variables
# 5. A few more things
#######################################

library(tidyverse)
library(Amelia)
library(stargazer)


#......................................
# 1. View example data ----
data(freetrade)
summary(freetrade)
table(freetrade$country)
# tariff, fiveop, intresmi, signed, polity

# model with listwise deletion
m1 <- lm(tariff ~ polity + pop + gdp.pc + year + country, data = freetrade)
summary(m1)


#......................................
# 2. Basic use ----
imp1 <- amelia(freetrade, m = 5, ts = "year", cs = "country")

# distribution of imputed tariff (from first imputed data set)
hist(imp1$imputations[[1]]$tariff)

# compare distributions of observed and (mean) imputed values
compare.density(imp1, var = "tariff")
# plot(imp1, which.vars = 3:4)

tscsPlot(imp1, cs = "Korea",  var = "tariff")

# tariff, fiveop, intresmi, 
# signed (Indonesia, Thailand, Sri Lanka), polity (Philippines, Korea)


#......................................
# 3. Incorporating time effects -- 
imp2a <- amelia(freetrade, m = 5, ts = "year", cs = "country", 
               polytime = 1)

compare.density(imp2a, var = "tariff")

tscsPlot(imp1, cs = "India",  var = "tariff")
tscsPlot(imp2a, cs = "India",  var = "tariff")

imp2b <- amelia(freetrade, m = 5, ts = "year", cs = "country", 
                polytime = 2)

compare.density(imp2b, var = "tariff")

tscsPlot(imp1, cs = "India",  var = "tariff")
tscsPlot(imp2a, cs = "India",  var = "tariff")
tscsPlot(imp2b, cs = "India",  var = "tariff")

imp2c <- amelia(freetrade, m = 5, ts = "year", cs = "country", 
                polytime = 2, intercs = TRUE)

compare.density(imp2c, var = "tariff")

tscsPlot(imp1, cs = "India",  var = "tariff")
tscsPlot(imp2a, cs = "India",  var = "tariff")
tscsPlot(imp2b, cs = "India",  var = "tariff")
tscsPlot(imp2c, cs = "India",  var = "tariff")

imp2d <- amelia(freetrade, m = 5, ts = "year", cs = "country", 
                lags = "tariff", intercs = TRUE)

compare.density(imp2d, var = "tariff")

tscsPlot(imp2a, cs = "India",  var = "tariff")
tscsPlot(imp2b, cs = "India",  var = "tariff")
tscsPlot(imp2c, cs = "India",  var = "tariff")
tscsPlot(imp2d, cs = "India",  var = "tariff")


#......................................
# 4. Transforming variables ----
# noms, ords
table(freetrade$polity)
table(imp2d$imputations[[1]]$polity)

table(freetrade$signed)
table(imp2d$imputations[[1]]$signed)

imp3a <- amelia(freetrade, m = 5, ts = "year", cs = "country", 
               lags = "tariff", intercs = TRUE,
               ords = "polity", noms = "signed")

table(imp3a$imputations[[1]]$polity)
table(imp3a$imputations[[1]]$signed)

# log, sqrt, lgstc
# log: heavily skewed variable, or outliers, can impact imputations;
# log transformation can normalize and prevent imputations that 
# depend heavily on outliers
imp3b <- amelia(freetrade, m = 5, ts = "year", cs = "country", 
                lags = "tariff", intercs = TRUE,
                ords = "polity", noms = "signed", log = "tariff")

compare.density(imp3a, var = "tariff")
compare.density(imp3b, var = "tariff")

# sqrt: commonly used for counts (generally skewed, must be positive)
# lgtsc: commonly used for proportions (sharply bounded)

# save all of the imputed data sets
# as an .Rdata file
save(imp3b, file = "freetrade_imp.RData")

# as separate csv files
write.amelia(obj = imp3b, file.stem="freetradeimp")


#......................................
# 5. A few more things ----
# model estimated on imputed data set
freetrade_imp <- imp3b$imputations[[1]]
m2 <- lm(tariff ~ polity + pop + gdp.pc + year + country, 
         data = freetrade_imp)
summary(m2)

# comparing m1 (listwise deletion) and m2 (imputated data)
stargazer(m1, m2, type = "text")
# but different imputed data sets might generate different inferences

# Could apply model to all imputed data sets and combine 
# (as multiple imputation intended, but less common when outcome model is predictive)

imp_amelia <- lapply(imp3b$imputations, as.data.frame)

# estimate model on each imputed dataset
m_amelia <- lapply(imp_amelia, lm, 
                  formula="tariff ~ polity + pop + gdp.pc + year + country") 
# pool results across imputed data sets
library(mice)
pool_amelia<-pool(as.mira(m_amelia)) 
summary(pool_amelia)


# Amelia also has diagnostic function, overimpute, a cross-validation approach: 
#   observed value is deleted, and 100 values are predicted by same approach,
#   the mean and confidence intervals are computed based on these values, and
#   see ifobserved value falls within the bootstrapped interval

overimpute(imp3b, var = "tariff")

#  The y=x line is where the imputations should fall if perfectly imputed, 
#   the mean and intervals for each value are shown. 
#   Around ninety percent of the confidence intervals should contain the y = x line.

overimpute(imp3b, var = "intresmi")
