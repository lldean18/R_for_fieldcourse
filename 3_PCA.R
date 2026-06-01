################################################
##   CHAMONIX BIODIVERSITY FIELD COURSE       ##
##             R tutorial  PCA                ##
################################################

# principal components analysis (PCA) can be a useful way
# to visualise patterns in continuous data and can also
# help to reduce multiple highly correlated variables 
# down to fewer uncorrelated axes that still capture most
# of the patterns in the original variables.

###########################
# SET UP YOUR ENVIRONMENT #
###########################

# don't worry too much about this it just ensures you are
# starting with a fresh R environment.
rm(list=ls()) # clear your global environment

## WARNING:: if you have already loaded packages from 
# another script since opening this window of R studio you
# should click Session > Restart R (at the top of this
# window) because some of the packages will cause errors
# if they are not loaded in a specific order (R is full of
# little quirks like this!)

# Now you have restarted R if need be you can load the 
# packages we will use into your working environment...
# you may get some warning messages about packages being
# built under older versions or R or some objects being
# masked from other packages, that's fine.
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggfortify)

###############################
# LOAD AND ORGANISE YOUR DATA #
###############################

# we will start by using one of the built in data sets in
# R called Iris... When you come to try this yourself
# you can read in your own data from excel using the 
# import data set button in the top right hand panel, in
# the same way as in the previous tutorials.
df <- iris # load the iris data set

# have a look at the first few lines of your data
head(df)

# this is a data set containing information about various
# different flower characteristics for three different 
# species of iris.

# Sometimes you will have data sets (like this one) where 
# there are lots of variables that are highly correlated
# with each other. You can check for this by looking at
# correlations in your data...

# Only numeric variables can be used in a correlation 
# analysis or PCA we can use the select_if() function to
# select only the numeric variables. The select_if()
# function takes the name of the data frame and the 
# condition we want it to meet when selecting variables
# as arguments... We will save the output of the 
# select_if() function to a new data frame called 'df_num'
df_num <- select_if(df, is.numeric)

# have a look at your new data frame, check which 
# variables remain...
head(df_num)

# now you have a data frame containing only numeric
# variables you can create a correlation matrix. We will
# create our correlation matrix using the corr() function
# which takes the numeric data frame as input. We will 
# round the numbers in the the correlation matrix to 2
# decimal places by passing it directly to the round()
# function and specifying 2 as the number of decimal
# places. We will save the correlation matrix to a new
# object called corr_mat...
corr_mat <- round(cor(df_num),2)

# have a look at your matrix
corr_mat

# We can plot the correlation matrix using ggplot
# don't worry about the code here but run it to look at
# the matrix you have generated
ggplot(melt(corr_mat), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

# We can see by looking at this plot that some of our
# variables are strongly correlated and others are not.

# if you have multiple highly correlated variables in
# your data set then including them all in a statistical 
# analysis such as a regression model would not be 
# sensible, as each additional variable would add 
# complexity to your model but not add much that is 
# useful statistically if it is strongly correlated with
# another variable you already have in your model.

# in this instance it can be useful to reduce the 
# variation in your data to fewer uncorrelated axes and
# this can be done using principal components analysis 
# (PCA).

# PCA in R is very straightforward using the prcomp()
# function. We pass it the name of the numeric only data
# frame, and we tell it we want it to center and scale
# the variables by including scale = T, center = T. We
# will save the output of the pca in an object called
# pca...
pca <- prcomp(df_num, scale = T, center = T)

# we can now summarize the pca using the summary()
# function...
summary(pca)

# you should see an output showing the standard 
# deviation, proportion of total variance explained, and
# the cumulative proportion of variance explained by 
# each the principal components (PC1, PC2, PC3... etc)
# Since PCA gives you the axes of greatest variation in
# your data you will see that the first principal 
# component (PC1) explains the greatest amount of 
# variation in the original data and the next principal
# component (PC2) the second greatest and so on.

# you can also access various elements of the pca object
# using the $ operator. For example, look at the loadings
# of your original variables for each PC:
pca$rotation

# loadings range from -1 to 1 with -1 implying the 
# variable is 100% negatively correlated with the PC axis
# 0 implying the variable is not correlated with the
# PC axis and 1 implying the variable is 100% correlated
# with the PC axis


# You can plot your new PC axes against each other and 
# see where each sample falls within PC space using the
# autoplot() function
autoplot(pca)

# We can see from this that it looks like there are
# multiple clusters in our data but it's not very easy
# to see what they are. Let's try colouring the points
# using some of the non-numeric variables from the 
# original data set. To do this we will pass the name of
# the original data set to the autoplot function using
# 'data =', and pass the column name of the variable we
# want to colour by using 'colour = "Variable name"'
# e.g...
autoplot(pca, data = df, colour = "Species")

# now we can see that the setosa species forms a very
# distinct group on PC1 and that versicolour and 
# virginica also differ (but to a lesser extent) along
# this axis.

# Finally we can add the loadings of each of our
# original variables to the plot by telling autoplot 
# to plot the loadings and their labels...
autoplot(pca, data = df, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# The loadings are displayed as arrows that show the
# correlation between each original variable and the
# two PC axes. The length of the arrow describes the 
# strength of the correlation. 

# This plot helps us to visualise the overall variance
# in our data set and is a nice way of summarising and
# exploring your data. it IS NOT a statistical test
# and can not be used as one, it is simply a descriptive
# exploration of your data, but a very useful one. In
# many scientific studies it is appropriate to take
# these PC axes and use them as variables in statistical
# models, but we won't go into that here.


###############################
# TRY THIS WITH YOUR OWN DATA #
###############################

# import your own data from excel (in the same way that
# we have done in previous tutorials).

# copy and paste lines of code from above to the bottom
# of this R script or to a new .R file and have a go
# at doing a PCA with your own data.

