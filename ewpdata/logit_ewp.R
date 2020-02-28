# .........................
# Saving Lives with Data II
# 2020-02-28, MPC
# Logit in R, Cross-Validation
# Exploring EWP Data
# .........................
# 0. Set up (packages, data)
# 1. Explore data
# 2. Logit/Classification
# 3. Predictions, confusion matrix, ROC curves
# 4. Training/Testing and Cross-validation


# .........................
# 0. Set up ----

# load packages
library(tidyverse)
library(ggeffects)
library(vip)
library(rsample) # for generating training/testing splits
library(caret) # for training models and cross-validation
library(pROC) # for ROC curves

# load data
# forked from https://github.com/EarlyWarningProject/2018-Statistical-Risk-Assessment
load("prepared_data_final_15Oct2018.RData")

# set up EWP data (from EWP GitHub)
# vector of outcome names 
outcomenames=c("anymk.start.1", "anymk.start.2window")

# vector of predictor names
predictornames = c("anymk.ongoing","anymk.ever",
                   "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", #americas, "reg.amr" left out as baseline.
                   "countryage.ln", "popsize.ln.combined", "imr.sqrt", "gdppcgrowth.combined",
                   "ios.iccpr1","includesnonstate",
                   "durable.ln","minorityrule", "elf.ethnic", "battledeaths.ln",
                   "candidaterestriction", "partyban","judicialreform",
                   "religiousfreedom", "pol_killing_approved",
                   "freemove_men4","freemove_women4", "freediscussion",
                   "social_inequality","even_civilrights","repress_civilsoc","social_power_dist", 
                   "tradeshare.ln.combined", 
                   "coup.try.5yr",
                   "polity2.fl.2","polity2.fl.3")


# .........................
# 1. Explore EWP data ----

# outcome variable: onset of mass killing in a 2 year window
table(dat$anymk.start.2window) %>% 
  prop.table() # 2.22%, a very rare occurrence

# outcomes by year
dat %>% group_by(year) %>% 
  summarize(events = sum(anymk.start.2window, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = events)) + geom_line()

# countries
table(dat$country_name) # these aren't all countries...?

# reduce to variables used by EWP
dat2 <- dat %>% 
  select(sftgcode, country_name, year, outcomenames, predictornames)
summary(dat2)

# generate factor version of outcomes for later
dat2 <- dat2 %>% 
  mutate(class = factor(anymk.start.2window, labels = c("No", "Yes")),
         class = fct_relevel(class, "Yes"))

# correlations among variables
# drop missing obs (which will be dropped from subsequent models in any case)
dat3 <- dat2 %>% drop_na  
# Note: this is what the EWP team does, but not the only option

# heatmap of correlation matrix
cor_mat <- cor(select(dat3, -c("sftgcode", "country_name", "year", "class"))) 
col <- colorRampPalette(c("blue", "white", "orange"))(100) # make a blue/white/orange color palette
heatmap(x = cor_mat, col = col, symm = TRUE)


# .........................
# 2. Logit ----

# Notes: Model the probability of event as a function of predictors
# but map the Xs to probability via logistic function:
#    Pr(Y=1) = e^(XB) / 1 + e^(XB)
# Any value of XB will generate a probability between 0 and 1

# logit model (minimal)
mk1 <- glm(anymk.start.2window ~ anymk.ever + reg.afr + reg.eap + reg.eur + reg.mna + reg.sca + 
             imr.sqrt + durable.ln + elf.ethnic, 
           data = dat3, family = "binomial")
summary(mk1)

# Notes: Sign and significance of coefficient estimates straightforward,
# but no linear interpretation. The coefficients represent the log of the odds:
#  ln(p/1-p), or possibly more helpfully, the exponentiated coefficients
# represent the multiplicative effect of a unit increase in X.
coef(mk1) %>% exp()
# A country that's experienced a prior mass killing event has increasing odds
# of another onset -- with odds increasingy mutiplicatively by 2.6 compared 
# to countries that have not experienced a past mass killing event. by 4.0812 for employees that work OverTime compared to those that do not.

# Deviance is a measure of model fit -- lower values indicate a better fit.
# Null deviance is the fit of an intercept-only model (guess the mean)
# Residual deviance is the remaining "error" in the estimated model -- if the 
# model is explaining any of the outcome, this should be noticeably lower.

# To understand the marginal effect of a variable, a plot can help
plot(ggpredict(mk1, terms = c("durable.ln [all]")))
# the probability of the onset of a mass killing event drops from ~ 2.5%
# for countries with a logged regime duration of 0 (1 year) to ~ 0.7% for countries
# with a logged regime duration of 5 (148 years).

vip(mk1) # based on the z-value


# For prediction, it is common to throw everything in the model -- let's try it.
# create a formula to make this easier
fm_mk2 <- as.formula(paste("anymk.start.2window ~ ", paste(predictornames, collapse= "+")))
fm_mk2

mk2 <- glm(fm_mk2, data = dat3, family = "binomial")
summary(mk2) # note the lower residual deviance and AIC compared to mk1

vip(mk2, 20, geom = "point")


# .........................
# 3. Predictions, confusion matrix, ROC curves ----

# generate (in-sample) predictions for mk1
dat3_mk1 <- broom::augment(mk1, dat3) %>% mutate(.fitted = exp(.fitted))
summary(dat3_mk1$.fitted)

# country-year, observed y, and (in-sample) predicted values
dat3_mk1 %>% arrange(desc(.fitted)) %>% 
  select(country_name, year, anymk.start.2window, .fitted) %>% head(20)

# map probabilities into predicted class
threshold <- 0.1 # define threshold

dat3_mk1 <- dat3_mk1 %>% 
  mutate(pred = if_else(.fitted > threshold, 1, 0), # numeric version
         pred_class = factor(pred, labels = c("No", "Yes")), # factor version
         pred_class = fct_relevel(pred_class, "Yes")) # make "Yes" first level

# A confusion matrix
table(pred=dat3_mk1$pred_class, ref=dat3_mk1$class)

# TP - 30: TN - 6647; FP - 204; FN - 139

# Notes:
# Accuracy: how often are the model predictions correct? 
#   (TP + TN)/N
(30 + 6647)/7020
# Precision: How accurately does the model predict events (1s)?
#   TP/(TP + FP) -- of all the onsets predicted, how many were correct?
30/(30 + 204)
# Sensitivity: How accurately does the model predict actual events?
#   TP/(TP + FN) -- of all the onsets that occured, how many were predicted?
30/(30 + 139)
# Specificity: How accurately does the model predict actual non-events?
#   TN/(TN + FP) -- of all of the non-events, how many did we predict?
6647/(6647 + 204)
# If correctly identifying positives is more important, then we care more 
# about Sensitivity; if correctly identifying negatives is more important, 
# then we care more about Specificity.


# or use caret::confusionMatrix
confusionMatrix(
  data = dat3_mk1$pred_class, 
  reference = dat3_mk1$class
)

# ROC/AUC
# Notes: Every threshold generates a different confusion matrix; a lower value
#   increases the number of False Positives, 
#   decreases the number of False Negatives.
# The ROC (Receiver Operator Characteristic) Curve plots the 
#   True Positive Rate (y-axis) against the False Positive Rate (x-axis).
# The ROC summarizes the confusion matrices produced for each threshold.

roc_mk1 <- roc(dat3_mk1, response = anymk.start.2window, predictor = pred)

plot(roc_mk1)

# The Area Under the Curve (AUC) -- the area between the ROC curve and the 
# diagonal line that represents random guessing -- is also used as a summary
# of a classification model. Higher values represent better combinations
# of sensitivity and specificity.
roc_mk1$auc

# Let's do it again for mk2
# generate (in-sample) predictions for mk1
dat3_mk2 <- broom::augment(mk2, dat3) %>% mutate(.fitted = exp(.fitted))
dat3_mk2 %>% group_by(anymk.start.2window) %>% summarize(mean(.fitted))

dat3_mk2 %>% arrange(desc(.fitted)) %>% 
  select(country_name, year, anymk.start.2window, .fitted) %>% head(20)

# map probabilities into predicted class
threshold <- 0.1 # define threshold

dat3_mk2 <- dat3_mk2 %>% 
  mutate(pred = if_else(.fitted > threshold, 1, 0), 
         pred_class = factor(pred, labels = c("No", "Yes")), 
         pred_class = fct_relevel(pred_class, "Yes")) 

# A confusion matrix
table(pred=dat3_mk2$pred_class, ref=dat3_mk2$class)

# or use caret::confusionMatrix
confusionMatrix(
  data = dat3_mk2$pred_class, 
  reference = dat3_mk2$class
)

# ROC/AUC
roc_mk2 <- roc(dat3_mk2, response=anymk.start.2window, predictor=pred)

plot(roc_mk2)
roc_mk2$auc

# compare the minimal model versus everything (using common threshold = 0.1)
plot(roc_mk1, col = "black", lty = 2)
plot(roc_mk2, add = TRUE, col = "blue")


# .........................
# 4. Training/Testing and Cross-validation ----
# For predictive analytics/machine learning, we want a model that predicts
# future (unseen) values accurately, not just one that fits past data.
# To understand the generalizability of the model, we split the data into
#   training data -- used to develop the model (features, algorithm, tuning parameters, etc.)
#   test data -- used to estimate the model’s performance, but not used in model development.
# e.g., randomly sample (or use a stratified sample) 60-70-80% of data for training.
# In cases of class imbalance (like this), stratitifed is better
# (though next week, we'll talk more about incorporating time).

# Split the EWP data
set.seed(121)
ewp_split  <- initial_split(dat3, prop = 0.7, 
                              strata = "anymk.start.2window")
train_ewp  <- training(ewp_split)
test_ewp   <- testing(ewp_split)

table(train_ewp$anymk.start.2window) %>% prop.table()
table(test_ewp$anymk.start.2window) %>% prop.table()


# To generate better estimates of the generalizability of the model, 
# we can use a cross-validation procedure -- estimate not just one metric
# on the training data, but split the training data again to create the
# training set and the validation/held-out set (to estimate performance).
# And instead of doing this once, we'll often do this multiple times.

# K-fold validation: divide training data into K groups, using K-1 groups
# to estimate the model, and the left out group to validate, and iterate
# so each K is held out for validation. Then average the validation metric
# across the K values.

# K-fold validation, using caret::train
cv_mk1 <- train(
  as.factor(anymk.start.2window) ~ anymk.ever + reg.afr + reg.eap + reg.eur + reg.mna + reg.sca + 
    imr.sqrt + durable.ln + elf.ethnic, 
  data = train_ewp, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
# there are other options for the validation method, e.g., 
#   boot (bootstrapping), LOOCV (leave one out cross validation)
cv_mk1


fm_mk2 <- as.formula(paste("as.factor(anymk.start.2window) ~ ", paste(predictornames, collapse= "+")))
fm_mk2

cv_mk2 <- train(
  fm_mk2,
  data = train_ewp,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

cv_mk2

# Model performance
# predict on test data
pred_class <- predict(cv_mk2, test_ewp, type = "prob")$`1`
test_ewp$pred_class <- pred_class

test_ewp <- test_ewp %>% 
  mutate(pred = if_else(pred_class > threshold, 1, 0),
         pred_class = factor(pred, labels = c("No", "Yes")),
         pred_class = fct_relevel(pred_class, "Yes"))

# create confusion matrix
confusionMatrix(
  data = test_ewp$pred_class, 
  reference = test_ewp$class)

