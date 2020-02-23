# 4c. Identify something about this data that isn't clear from the spreadsheet documentation?

# Note: A key takeaway is the importance of complete documentation for replicability. Something
# we will want to take seriously in our work.

# ....
# Question: This model does not seem  to have any accuracy or test stats. Even if we only have the data 
# at hand, we could use cross alidation to create test/validation sets. These values are
# important to include in the sheet so we can know whether we need to tune our list of variables.

# Response: I don't fully understand this as a critique of the data set, as we haven't yet done
# or seen any models. To be sure, we are not fully aware of the details of the models the BCG 
# used for SCI.

# ....
# Q: For some of the numbers on a scale (such as fragility index), it isn't clear what a high 
# or low value would be since we aren't able to have a comparison to a country like the U.S, 
# U.K, or Norway.  

# R: The original data source - the Center for Systemic Peace -- provides more thorough
# documentation for how these values are generated and metrics for all countries. The data 
# definition page also provides the range, from 0 (no fragility) to 25 (extreme fragility).

# ....
# Q: "Ongoing" is the value of several columns where its meaning is not defined in the definitions. 
# Some columns also have "NA". Is there a difference between ongoing and NA?
# Some fields also have missing values.

# R: which fields, specifically, have NAs and ongoing? The ongoing_duration is missing for all
# of the events that are not ongoing.

# ....
# Q: How do they account for variation in data quality? Some of these sources are "mainly Wikipedia," 
# including destruction of infrastructure and roads. How reliable is that data? 
# And what is the specific metric? Miles of road destroyed? Loss in transportation? It is unclear.

# R: a good question -- Wikipedia is not a replicable source witout clearer instructions for 
# what to look for. But the destruction variables are all yes/no, so the question is more about
# how much destruction is required to receive a "yes".

# ....
# Q: I'm not clear on how they're defining when the original conflict ended, or what defines new 
# conflicts. Really everything about duration is still a little fuzzy for me.

# R: ?

# ....
# Q: I am really frustrated by the "Calculation" column in the Data Collection Status tab.
# It is not intuitive at all how they are doing these calculations, they are not transparent 
# at all how the calculation is taking place. Instead, it is just 1-2 words that are sometimes
# not intuitive or self evident at all, and raise far more questions than they answer.

# R: Maybe some concrete examples would help here. I'm seeing mostly (1) use the value in year 
# prior to conflict, (2) use the most recently available value, (3) use the average of multiple
# years.

# ....
# Q: The ethnic composition category is somewhat ambiguous when they define it as dominant, mixed, 
# and 2+major. Also, the fragility index and polity score seems very subjective and how it is
# determined is somewhat confusing. 

# R: The fragility and polity scores come from the Center for Systemic Peace, who more fully
# detail their construction. The data definition for ethnic composition defines the cutpoints,
# at least, if not the logic behind the cutpoints.

# ....
# Q: The scale model includes infrastructure destruction in its list of metrics, but 
# it seems arbitrary what constitutes infrastructure destruction or not.
# It is broken down into categories like bombings, roads, utilities, etc, and 
# sourced almonst exclusively from Wikipedia. There is no mention of what
# quantity or level of infrastructure destruction warrants a 'yes', and it may 
# be more useful of an indicator as a discrete variable. 

# R: Agreed (as above).
