################################################
##        BIODIVERSITY FIELD COURSE           ##
##    R tutorial: Plotting using ggplot       ##
################################################

# For more information on data manipulation and a
# general (very good) introduction to R, have a look
# at this page:
#] https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf


###########################
# SET UP YOUR ENVIRONMENT #

# don't worry too much about this it just clears 
# anything you have previously loaded into your
# environment and modified i.e. datasets and is
# good practice when you start a new session.
rm(list=ls()) # clear your global environment

## WARNING:: if you have already loaded packages from
# another script since opening this window of R studio 
# you should click Session > Restart R (at the top
# of this window) because some of the packages will
# cause errors if they are not loaded in a specific 
# order (R is full of little quirks like this!)

# Now you have restarted R if need be you can load the
# packages we will use into your working environment...
# you may get some warning messages about some objects
# being masked from other packages, this just happens
# when functions from different packages are called the
# same thing and for our purposes it won't cause a problem.
library(readxl)
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)

##################################
# READ IN AND ORGANISE YOUR DATA #

# Import the practice data from excel to a data frame 
# called 'df' using the 'Import Dataset' drop down menu
# in your environment box in the upper right corner of
# your R-studio window. Select ' from excel'. In the 
# box that opens you will see there is a button in the
# top right corner that says browse. Click this button
# and find the data file df.xlsx. Click import.

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

# most things in R are done using functions. Functions are
# passed arguments inside a pair of curly brackets. For
# example if you pass the View() function the name of
# your data frame it will open it as a tab for you so
# that you can view it...
View(df)

# look at the practice data set, you have data for three
# sites at three different elevations. At each site data
# has been collected using various methods e.g. sweep 
# netting or quadratting. You then have counts for the
# number of each species that was recorded at each of the
# three sites. This data is similar to the data you might
# be collecting on this field course.

# you can use some simple functions in R to look at various
# aspects of your data frame. For example...

# you can look at the maximum, the minimum, the range and
# the mean of a continuous variable using these simple
# functions...
# by default the output of most functions will be printed to
# the console below.
# This time you must pass each function both the name of 
# your data frame and the name of the variable you want 
# it to consider. Each variable (column header in your
# data frame) can be accessed by typing the name of the
# data frame followed by a $ symbol and then the variable
# name e.g...
max(df$Elevation) # maximum elevation
min(df$Elevation) # minimum elevation
range(df$Elevation) # min & max elevation
mean(df$Elevation) # mean elevation

# OR
max(df$Count) # maximum count
min(df$Count) # minimum count
range(df$Count)  # min & max count
mean(df$Count) # mean count

# commonly variables are categorised in R as either 'numeric',
# or 'character' / 'factor', which are both categorical and
# essentially the same thing but are treated slightly differently
# in R because a factor can be composed of numbers as well as
# strings (text).

# you can check what R has classified each variable in your data
# frame as using the class() function...
class(df$Elevation)
class(df$Count)
# Elevation and Count are both numeric as we would expect
class(df$Method)
class(df$Genus)
class(df$Weather)
# Method, Genus and Weather are all characters.

# for character variables you can see the different
# categories you have recorded using the unique function...
unique(df$Method)
# as you might expect, you have three different categories
# for your method variable.
# Now look at the weather variable...
unique(df$Weather)
# can you see a problem here?
# You have two different categories for cloudy and sunny
# and that is because whoever has entered the data into
# your excel sheet has not been consistent with their
# category names... some are capitalised and others aren't.
# R is case sensitive so you must be careful to enter
# data into your spreadsheet consistently. If you encounter
# this problem with your own data, go back to your
# spreadsheet and change the values so they are all consistent
# then re-load the data into R.

# If your variables are read into R as a different class than
# you intended, you can coerce them to the class you want
# them to be, providing they are in the correct format.
# By default R prints the output of most functions to the 
# console below, but if you want to save that output as an
# object instead you can use the <- operator.
# e.g. you can change a numeric variable to a character by
# assigning the output of the as.character() function to
# the variable...
class(df$Elevation) # first check which class your variable is
df$Elevation <- as.character(df$Elevation) # overwrite the old
# variable with the new one, which we have made a character.
class(df$Elevation) # check what class it is now
# then you can change it back again using the as.numeric()
# function...
df$Elevation <- as.numeric(df$Elevation) # change it back
class(df$Elevation) # check the class again


#######################
# filtering your data #
#######################

# if you want to focus on certain aspects of your
# data or remove certain entries for some reason
# you can easily filter it within the R environment

# it is a good idea not to save over your original 
# data,even within your R environment, but instead save 
# your modifications to a new data frame. REMEMBER
# if you do accidentally overwrite your original data
# you can always re-load it as R does not save changes
# you make outside of you R session (unless you 
# specifically tell it to).

# if you want to remove rows from your data frame that
# contain NA values, you can use the na.omit() function
# e.g...
df_no_na <- na.omit(df)
# but bear in mind that if you have any blank entries
# at all in your excel file, these will be read into
# R as NA. For example, all of the entries in our
# practice data frame that did not have an entry in
# the notes column have now been removed in the new
# data frame we created, so we are only left with 
# 3 observations (rows). You can see how many 
# observations and how many variables you have in each
# data frame in your environment by looking in your
# 'global environment' panel in the top right.

# if we didn't want it to count the notes column, we 
# could remove it first, using the subset function. 
# If we wanted to keep only the Notes column, we could
# write select = Notes, but we include the minus
# symbol because we want to remove the named column...
df_no_na <- subset(df, select = -Notes)
# now we can remove NAs without worrying about the
# notes column
df_no_na <- na.omit(df_no_na)
# and we actually don't have any other NAs in this
# data set so now we don't loose any more observations
# (rows)

# We can also use the select function to pick out
# more than one column to keep or remove by putting
# all of the column names inside the c() function.
# c stands for combine e.g...
df_small <- subset(df,
          select = c(Method,Genus,Species,Count))

# you can also use the filter() function from the dplyr 
# package to make new versions of your data frame 
# that are filtered by certain conditions that you 
# specify e.g.....

# keep only entries where the sweep netting method was
# used. The == symbol means keep all entries that are
# equal to
df_sweep_net <- filter(df, Method == "sweep netting")

# look at the new data frame you have created by
# clicking on it in your environment (upper right hand
# box in this window). Notice that now you only have
# entries that were collected using sweep netting.

# try keeping only entries that were not collected by
# sweep netting. The != symbol means keep all entries
# that are not equal to...
df_not_sweep_net <- filter(df, Method != "sweep netting")

# look at this data frame and check the methods column,
# all entries that were collected by sweep netting have
# not been included in this new data frame.


###################
# GGPLOT TUTORIAL #
###################

# lets try making some simple plots of the data.

# In ggplot, you first set up the variables that
# correspond to different "aesthetics". As a minimum,
# these usually include the x and y axes, but can also
# include things like colour, shape of points, style of
# lines, etc.
ggplot(df, aes(x = Elevation, y = Count))

# We have set up the axes, but nothing has been plotted
# because we have not specified what form our data
# should be displayed in. This is known as a "geometric
# object" or "geom". We can add geoms to the axes that
# we have set up.

# x-y scatter plots are useful if you want to look
# for a relationship between two continuous variables.
# we plot a scatter plot using geom_point()...
ggplot(df, aes(x = Elevation, y = Count)) +
  geom_point()
# now you can see the relationship between the number
# of individuals recorded and the elevation.

# If you want to plot the above relationship with a 
# line instead you can do that using geom_smooth.
# (here we tell ggplot to plot a straight
# line by passing "method=lm" to geom_smooth and not to
# plot standard errors by passing it "se=FALSE")
ggplot(df, aes(x = Elevation, y = Count)) +
  geom_smooth(method=lm, se=FALSE)

# If we want we can have points and a line, just
# include both geom_point() and geom_smooth() in the
# same plot
ggplot(df, aes(x = Elevation, y = Count)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

# We can also change many aspects of the plot by 
# changing the theme. There are also many default
# themes to choose from e.g.
ggplot(df, aes(x = Elevation, y = Count)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  theme_classic()

# OR
ggplot(df, aes(x = Elevation, y = Count)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()

# if you start typing the word theme, R studio will
# show you the different auto complete options (if 
# it doesn't put your cursor at the end of the word
# theme, below and press the tab key)... Have a look
# at some of the different themes yourself...
ggplot(df, aes(x = Elevation, y = Count)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  theme


# If you are interested in comparing groups within a 
# categorical variable you will need to use a bar chart
# or box plots.

# For boxplots we use geom_boxplot()
ggplot(df, aes(x = Genus, y = Count)) +
  geom_boxplot()

# For bar charts we use geom_bar()
# by default, geom_bar() requires only a variable
# for the x-axis because by default it will plot a 
# histogram (i.e. a frequency bar chart of a single
# variable)
ggplot(df, aes(x = Count)) +
  geom_bar()

# if we want it to plot two variables against each
# other rather than just a histogram we need to
# include the argument stat = "identity" inside
# geom_bar() and include a y-axis variable in our
# aesthetic argument.
ggplot(df, aes(x = Genus, y = Count)) +
  geom_bar(stat = "identity")

# if we want to plot multiple groups at each point
# on the x-axis we can tell ggplot that we want to
# plot the bars next to each other rather than stacked
# on top of each other by setting the position to
# position = "dodge" inside geom_bar()
# We then specify the group inside the aes brackets...
ggplot(df, aes(x = Genus, y = Count, group = Site)) +
  geom_bar(stat = "identity", position = "dodge")
# this is ok but we can't see which bar is which
# to make things clearer we can tell it to fill the
# bars with different colours depending on one of our
# variables e.g.
ggplot(df, aes(x = Genus, y = Count, group = Site, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge")

# you can change the theme for box and bar plots in 
# exactly the same way with theme...
ggplot(df, aes(x = Genus, y = Count, group = Site, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic()


# With large data sets, it can be really useful to split
# up your data into several panels...
# you can do this using facet wrap e.g... 
# We tell ggplot which categories we want it to use for
# the panels by giving it a categorical variable that
# follows a ~ symbol in the facet_wrap geom
ggplot(df, aes(x = Genus, y = Count)) +
  geom_boxplot() +
  facet_wrap(~ Site)
# notice how the x-axis text is overlapping and
# unreadable you can fix this by editing the theme
# manually and changing the angle of the text e.g...
ggplot(df, aes(x = Genus, y = Count)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  theme(axis.text.x = element_text(angle = 90))


# Finally, making your plot pretty!! :)
# You can use other "aesthetics" to code for different
# variables e.g...
# Colour
ggplot(df, aes(x = Elevation, y = Count, colour = Site)) +
  geom_point()
# Shape
ggplot(df, aes(x = Elevation, y = Count, shape = Site)) +
  geom_point()
# Colour and shape with the same variable
ggplot(df, aes(x = Elevation, y = Count, colour = Site, shape = Site)) +
  geom_point()
# Colour and shape with different variables
ggplot(df, aes(x = Elevation, y = Count, colour = Genus, shape = Site)) +
  geom_point()

# You can customise almost any aspect of your plot that
# you could possibly imagine. Most of these customisable
# features are accessed using different elements within
# theme...
ggplot(df, aes(x=Elevation, y=Count, colour=Site, group=Site, shape = Site)) +
  geom_point(size = 3, alpha=1) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black", linewidth = 0.5),
        axis.title.x = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        text = element_text(size = 10)) +
  xlab("Elevation (m)") +
  ylab("Count") +
  scale_color_manual(values=c("blue", "red", "black"))
  
# OR for a bar chart...
ggplot(df, aes(x=Genus, y=Count, group=Site, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black", linewidth = 0.5),
        axis.title.x = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black", angle = 90),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        text = element_text(size = 10)) +
  xlab("Genus") +
  ylab("Count") +
  scale_fill_manual(values=c("blue", "red", "black"))

############################################
# HAVE A GO AT INVESTIGATING YOUR OWN DATA #

# Now try having a go at analysing your own data ...
# import your data from excel just like you did with the
# practice data

# copy and paste bits of code from above to the bottom 
# of this script or to a new R script and play around 
# with them.

# remember you will need to change the name of the data
# frame in the code from 'df' to whatever you have 
# called yours to plot your own data.

# you will also need to change the names of the variables
# you wish to what ever you have called them in your
# data frame (remember R is case sensitive).








