################################################
##        BIODIVERSITY FIELD COURSE           ##
##     R tutorial statistical analyses        ##
################################################

###########################
# SET UP YOUR ENVIRONMENT #
###########################

# don't worry too much about this it just ensures you
# are starting with a fresh R environment.
rm(list=ls()) # clear your global environment
options(scipen = 999) # turn off scientific notation
# (if you don't do this you will find it very difficult
# to read your p-values!)

## WARNING:: if you have already loaded packages from 
# another script since opening this window of R studio 
# you should click Session > Restart R (at the top
# of this window) because some of the packages will
# cause errors if they are not loaded in a specific 
# order (R is full of little quirks like this!)

# Now you have restarted R if need be you can load the
# packages we will use into your working environment...
# you may get some warning messages about packages being
# built under older versions or R or some objects being
# masked from other packages, that's fine.
library(readxl)
library(plyr)
library(dplyr)
library(faux)
library(effects)
library(MASS)
library(emmeans)
library(rcompanion)


##############################
# LOAD AND EXPLORE YOUR DATA #
##############################

# Read in the df_plant.xlsx data file you have been
# given using the 'Import Dataset' drop down menu
# in your environment box in the upper right corner of
# your R-studio window. Select ' from excel'. In the 
# box that opens you will see there is a button in the
# top right corner that says browse. Click this button
# and find the data file df_plant.xlsx. Click import.

# you should now see your data frame in your
# environment in the top right section of your R-studio
# window. You can look at your data frame by clicking
# on it. This will open the data frame as a tab in
# this window. You can look at your data, check things
# look right and then close that tab above.

# IMPORTANT - when you come to read in your own data
# it will, by default, be named in R using the name of
# the excel file. R does not deal well with spaces so
# make sure with your own data that your spreadsheet 
# has a simple name with no spaces and that your variable
# names also have no spaces.

# remember you can access individual variables within
# data frames by typing the name of the data frame
# followed by the $ symbol and then the name of the
# variable. R will then print that variable to the
# screen e.g...
df_plant$leaf_length
df_plant$leaf_width
# this can be useful when you want to refer to a 
# specific variable in an analysis e.g. a t-test.

# it can also be useful to have a little investigate of
# how each of your variables are distributed as this 
# might influence how you want to analyse them...
# you can plot the distribution of your variables in a
# histogram using the hist() function like this:
hist(df_plant$leaf_length)
hist(df_plant$leaf_width)
hist(df_plant$plant_height)
hist(df_plant$leaflets_per_leaf)
hist(df_plant$aphids_per_leaf)
# obviously you cannot plot categorical variables like 
# this and if we try to R will give us an error message...
hist(df_plant$leaf_colour)
# but if R has read in our categorical variable as 
# numeric (or of we force it to using the as.numeric()
# function) it will still produce a (rather strange) 
# plot e.g.
df_plant$is_deciduous <- as.numeric(df_plant$is_deciduous)
hist(df_plant$is_deciduous)
# We can change the class of our variable back to
# character (because realistically that is what it should
# be, a plant can only be deciduous or evergreen, it 
# cannot be in between)
df_plant$is_deciduous <- as.character(df_plant$is_deciduous)
# now when we try to plot the histogram we will get
# an error that the variable we have tried to plot is not
# numeric
hist(df_plant$is_deciduous)


########################
# STATISTICAL ANALYSIS #
########################

# You will probably have come across various statistical
# tests for different types of data e.g. t-tests /
# ANOVA / ANCOVA etc. In reality you can do the
# equivalent of all of these tests using a general or
# generalised linear model or glm. Below I will give you 
# the code to perform different types of glm depending on
# what type of data you have and what question you want 
# to ask of your data. Run through all of the code in the 
# first instance to see how the tests all work.

# When you come to analysing your own data you need to
# think about the type of data you have and the
# question you want to ask and decide which analysis
# is appropriate to you. You will then use ONE of the 
# tests you have learned here.


#####################################
# Comparing the means of two groups #
#####################################

# You can use a simple glm with a normal error
# distribution to test for differences between the means
# of two groups (much like a t-test). Our null
# hypothesis is that there is no difference between 
# the means of the two groups so if we get a 
# significant p-value, we can reject that
# hypothesis and conclude that the two groups differ.
# The function glm() takes three arguments: the equation,
# which is the predictor and response variables you want
# to test, in this case your response variable will be
# categorical with two categories, the name of the data 
# frame and the family (where your response variable is 
# normally distributed, this will always be "gaussian").
# we save the output of the model to an object called
# two_groups, which now exists in our global environment
# (top right) until we close this R session or restart R.
two_groups <- glm(plant_height ~ is_deciduous,
                  data = df_plant, family = "gaussian")

# You can visualise the effects of the predictor 
# variables in your model using the allEffects() 
# function inside the plot() function. We must specify
# the name of the model we want to plot. This is the
# object called two_groups that we have just saved e.g...
plot(allEffects(two_groups))

# Does it look like deciduous and evergreen plants differ
# in height?

# We can then test whether the removal of the predictor
# variable (is_deciduous) significantly affects the fit of
# the model to the data using an F test with the step()
# function
step(two_groups, test = "F")

# look at the information that is output in the R console
# below... the important information here is the 
# degrees of freedom (df), the F statistic and the
# p-value (called Pr(>F)) for your model term 
# 'is_deciduous'. These are the three things you 
# would need to report in a paper or in your presentation.
# HINT: You might need to scroll up a bit in the console
# below to see them.

# did plant height differ between deciduous and evergreen
# plants?


###############################################
# COMPARING THE MEANS OF MORE THAN TWO GROUPS #
###############################################

# glm can also compare the means of more than two groups
# (much like an ANOVA). This is the same as comparing the
# means of two groups, except our categorical predictor
# variable has more than two categories. e.g...

many_groups <- glm(plant_height ~ leaf_colour,
                   data = df_plant, family = "gaussian")

# again we can visualise the effects of our variables
plot(allEffects(many_groups))

# and use the F test to see whether the removal of our
# predictor variable significantly affects the fit of
# the model.
step(many_groups, test = "F")

# Did plant height vary with leaf colour? (i.e. is the
# p-value statistically significant?)

# If you get a statistically significant p-value from
# your glm, this tells you that at least one of the
# group means you have tested differs from the others.
# IT DOES NOT TELL YOU WHICH ONES ARE DIFFERENT. To find
# that out you need to do a simple 'post-hoc' test,
# which will compare every possible pair of groups with
# each other and give you a p-value for each pair-wise
# comparison. For this we will use the emmeans() 
# function. We pass the function three arguments. The
# name of our model (many_groups), the comparisons we 
# want it to test as a pairwise list, and the test we 
# want it to perform (in this case a Tukey test).
emmeans(many_groups, list(pairwise ~ leaf_colour),
        adjust = "Tukey")

# Look at the output. The important things here are the
# t.ratio statistic, the degrees of freedom and the 
# p.value for each pairwise comparison.

# Did all of the plants of different leaf colour
# differ significantly in height?


#####################
# Linear regression #
#####################

# You can also use a simple glm with a normal error
# distribution to test for a linear relationship between
# two variables. Our null hypothesis here is that there is
# no linear relationship, so if we get a significant 
# p-value, we can reject this null hypothesis and conclude
# that the linear relationship is significant. The 
# arguments to the glm() function are the same as before,
# but our predictor variable is now continuous rather
# than categorical. e.g. to test whether leaf width is
# related to leaf length...
linear_relationship <- glm(leaf_width ~ leaf_length,
                  data = df_plant, family = "gaussian")

# Again, you can visualise the effects of the predictor 
# variables in your model using the allEffects() 
# function inside the plot() function. e.g...
plot(allEffects(linear_relationship))

# you can then test whether the removal of your predictor
# variable (leaf width) significantly affects the fit of
# the model to the data using the F test
step(linear_relationship, test = "F")

# Again, the important bits are the test statistic (in
# this case F), the degrees of freedom and the 
# p-value (which is called Pr(>F))

# was there a significant linear relationship between 
# leaf width and leaf length?

############################################
# INTERACTIONS BETWEEN PREDICTOR VARIABLES #
############################################

# You can also include both categorical and continuous
# predictor variables in your model at the same time
# and choose to test whether there is an interaction
# effect between them i.e. "regression with different
# slopes". To do this, we can use the same glm() function
# but will add another predictor variable, this time
# categorical to our equation. We will use an * between
# predictor variables in the equation to include both the
# variables and their interaction in the model. If we 
# only want to include the variables but not test for
# an interaction, we would use a + symbol between 
# variables instead.
different_slopes <- glm(leaf_width ~ leaf_length * leaf_colour,
                           data = df_plant, family = "gaussian")

# Again, you can visualise the effects of the predictor 
# variables in your model e.g...
plot(allEffects(different_slopes))

# you can then test whether the removal of your predictor
# variable (leaf width) significantly affects the fit of
# the model to the data using the F test
step(different_slopes, test = "F")

# Look at the output in the console below. The 
# interaction term is referred to as leaf_length:leaf_colour
# Again, the important bits are the test statistic (in
# this case F), the degrees of freedom and the 
# p-value (which is called Pr(>F)). Look to see if there is
# a significant interaction between these terms. If there
# is, this tells you that the slope of the regression of
# leaf length and leaf width differs for plants with 
# leaves of different colour.

# if the interaction term is statistically significant in
# your model then both predictor variables must obviously 
# also remain in the final model. If you want to get 
# p-values for those terms in their own right, you will
# need to create a simplified model without the interaction
# term and use that in the step function. Remember, to 
# leave the interaction term out of the model, we
# use the + instead of * symbol between terms e.g...
simplified <- glm(leaf_width ~ leaf_length + leaf_colour,
                  data = df_plant, family = "gaussian")

# then you can run the step function on this simplified
# model...
step(simplified, test = "F")

# Did leaf length and leaf colour vary significantly with
# leaf width as standalone terms?

# You can choose to include as many terms and interactions
# as you like in your model, but it is best to keep things
# simple in most circumstances and stick to the key 
# things you need to answer the question that you decide
# to ask of your data. Remember each term in the model
# will use up valuable degrees of freedom!


##################################
# Checking the fit of your model #
##################################

# All of the models you have seen so far have specified
# a normal (gaussian) error distribution.

# You can look at how well a Gaussian model fits your
# data by checking the distribution of the residuals
# of your model using the qqnorm() function. We pass the
# function the name of your model followed by a $ and the
# word residuals as an argument. Let's try this with one
# of the models we made earlier...
qqnorm(many_groups$residuals)
# we follow it with qqline to add a line to our plot
qqline(many_groups$residuals)

# examine your normal Q-Q plot. If your points fall
# reasonably close to the line then the residuals of your
# model are normally distributed and you are good with a 
# gaussian model.

# lets look at some of our other models...
# different slopes looks good
qqnorm(different_slopes$residuals)
qqline(different_slopes$residuals)

# simplified looks ok...
qqnorm(simplified$residuals)
qqline(simplified$residuals)

# two groups is a bit off so with this one we
# might want to try some models with different
# error distributions using the code in the 
# section below to see if we can get the fit to
# look a bit better (although you can't always)
qqnorm(two_groups$residuals)
qqline(two_groups$residuals)

# same with linear relationship
qqnorm(linear_relationship$residuals)
qqline(linear_relationship$residuals)



#############################
# Generalized linear models #
#############################

# GLMs are extremely flexible and allow you use many
# different residual error structures if your data do
# violate the assumptions of normality. We don't want 
# you to get too hung up about this on the field course
# but if you are clearly modelling a poisson or 
# binomial response variable it would be worth comparing
# the fits of models with different error distributions
# and seeing which one fits your data the best.

# Technically it is only the residuals of the fit of your
# model that should be normally distributed, the response
# variable itself does not actually have to have a normal
# distribution for a gaussian model to provide the best
# fit, but it can still be a good indicator of which model
# you should try.

# remember, you can look at the distribution of your
# chosen response variable very easily using the hist()
# function...
hist(df_plant$leaf_length) # normal, usually numerical 
# measurements
hist(df_plant$leaflets_per_leaf) # poisson, usually count
# data
# binomial variables are those in which there are only
# two possible outcomes.. e.g. is the tree deciduous or
# not or is a species present or not. As long as these 
# variables are coded as 0 or 1, and classified as numeric
# you can plot them with a histogram e.g.
df_plant$is_deciduous <- as.numeric(df_plant$is_deciduous)
hist(df_plant$is_deciduous)

# POISSON GLM #
# lets try modelling a poisson response variable twice, 
# once with a gaussian error structure and once with a
# poisson and then compare them to see which model is a 
# better fit to the data. We tell R to use a poisson 
# distribution by specifying family = "poisson"
gaussian <- glm(leaflets_per_leaf ~ leaf_width,
               data = df_plant, family = "gaussian")

poisson <- glm(leaflets_per_leaf ~ leaf_width,
                data = df_plant, family = "poisson")

# now we will use the compareGLM() function to compare the
# two models. 
compareGLM(gaussian, poisson)

# look at the fit criteria in the output - which model
# has a lower (which is better) AIC? In this case, the 
# model with a poisson error distribution has a lower AIC
# so this should be the model you use.

# now that we have decided we should use a poisson model
# in this particular case we can use the step function
# in the same way as before to test for the significance 
# of terms in the model. With a poisson model we will use 
# the likelihood ratio test rather than the F test by
# specifying test="LRT"...
step(poisson, test="LRT")

# Was there an effect of leaf width on the number of 
# leaflets per leaf?


# BINOMIAL GLM #
# Like we said before, binomial variables are those in
# which there are only two possible outcomes.. e.g. is
# the tree deciduous or not or is a species present or
# not. Since you know that data that can only be 0 or 1
# it should be modeled using a binomial error structure.
# we will specify this using family = "binomial"...


# A binomial model will not work with a variable that
# is classed as a character so check that your response
# variable is either numeric or a factor and change the
# class to factor if necessary
class(df_plant$is_deciduous) # check the class
# update the class if necessary
df_plant$is_deciduous <- as.numeric(df_plant$is_deciduous)

# now we can create our binomial model
binomial <- glm(is_deciduous ~ leaf_width + plant_height,
                data = df_plant, family = "binomial")


# Again we will use the step function to test for the
# significance of terms in the model. Again, we will use 
# the likelihood ratio test rather than the F test by
# specifying test="LRT"...
step(binomial, test="LRT")

# did either leaf width or plant height differ between
# deciduous and evergreen trees?


#########################
# READ IN YOUR OWN DATA #

# Now try having a go at analysing your own data ...
# import your data from excel using the import data
# button, like you have been doing with the practice
# data sets.

# You can copy and paste bits of code from above to the
# bottom of this script or to an new R script and play 
# around with them. It is best to copy and paste for 
# your own data before editing the code in case you 
# make a mistake or something doesn't work, you can 
# always go back and compare to the course code.

# remember you will need to change the name of the data
# frame in the code from 'df_plant' to whatever your
# excel spreadsheet is called to plot your own data.
# You will also need to change the variable names to plot
# different variables (and remember, R is case sensitive)

# think about the data you want to analyse.
# what would be the best statistical test for the 
# question or questions you want to ask of your data?
# you will need to work this out to provide statistical
# support for the answer to the question you have 
# decided to investigate.






