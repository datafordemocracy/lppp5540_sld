# .........................
# Saving Lives with Data II
# Problem Set 1
# Wrangling and Regression in R
# Exploring SCI Data
# .........................


# Working further with Save the Children International's current data, in this problem set
# you'll make some additional changes to the data, generate visualizations, and extend the
# modeling we begain in class. Be sure to save this R file with your initials, write (and 
# execute) code to generate changes/figures/models, and add some verbal explanation using comments.


# .........................
# 0. Read in the SCI data, load necessary libraries ----
#  Use the the revised rds data frame we created during class.


# .........................
# 1. Additional wrangling ----
# a. mutate ethnic_comp, country_income, and ext_supp into factors;
#   set the sequences of the resulting levels with intention.


# b. locate the remaining 2-category variables that are currently formatted as characters
# (e.g., containing yes, no responses) and reformat them all as factors -- ideally all together.


# c. As you proceed to the next parts of the problem set, you might find you want to use another
# variable that first needs to be reformated (or recoded, or releveled); 
# if so, add those changes here.


# .........................
# 2. Visualization ----
# Focusing on peak size of a displacement event as the outcome of interest, visualize the
# relation of peak size and FIVE different potential predictor variables (these should be
# variables we could plausibly use to predict size, so should not be a function of size or
# a characteristic we would not know ahead of time (e.g.,duration, conflict end)). 
#    Make sure at least one of the potential variables is numeric and at least one is a 
#    factor/categorical (as these should be visualized with different kinds of figure types).
#    For at least one figure (more if you like), add in additional information by mapping
#    color, size, or shape.
# After each figure, add some explanation of what you are seeing.


# .........................
# 3. Linear modeling ----
# a. Continuing to focus on peak size as the outcome of interest, generate THREE additional 
# linear regression models that expand on what we worked on in class. These might include
# the incorporation of additional and different predictors, additional or different 
# transformations of the predictors or the outcome, additional interactions, and the like. 
#    For each model you submit, verbally indicate which variables appear to be most important 
#    in the model; and use the model to simulate outcomes and consider how well the model 
#    does in replicating the outcomes we observe in this data set. Which of your three models
#    do you prefer

