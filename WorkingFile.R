#Set up session####

#Set up your workspace
rm(list=ls()) #remove debris from 'Environment'

# Set your working directory 
# (manually use these)
getwd()
setwd(dir = "/Users/Dudisimo/Documents/PhD/TAing/Rworkshop_MBE2017") #diferent for each machine
# or by cliking 'Session' -> 'Set working directory'

# Install (if required) and load packages needed
install.packages("ggplot2")
install.packages("doBy")

# If installed or after installing them, load them in each R session (when using them)
library("doBy")
library(ggplot2)

#A custom theme for ggplot
#This only creates a set of instructions that R will feed to ggplot latter on
theme_gg <- function(base_size = 11, base_family = "") {
  theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey10"),
      axis.text = element_text(colour = "grey20",size=12),
      axis.title = element_text(colour = "grey20",size=16),
      legend.title = element_text(colour = "grey20", size = rel(1.2)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 1),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(size = rel(1), colour = "grey20"),
      legend.key = element_rect(colour = NA)
    )
}

# Read in data####
cukes <- read.csv("data/orange.cukes.csv",  stringsAsFactors = T, header=T) #factor, character with a certain number of categories
str(cukes) #Always a good idea to explore first your data to check for tidyness
View(cukes)
summary(cukes) #Basic summary stats on data

#Visualize data####
#Remember that plots take data in the form "independent variable (x), dependent variable (y)"
#Visualize all your data in a quick line
plot(cukes)

#Visualize categorical variables
#These plots show continuous vs. categorical data as a box and whisker plot, 
#where the middle line is the mean
plot(cukes$orange_cukes ~ cukes$Depth)
plot(cukes$orange_cukes ~ cukes$Students)

#The basic x ~ y plot for continuous variables
plot(cukes$rugosity, cukes$orange_cukes) 

#From these early visualizations we can hypothesize that there are differences between 
#categorical variables. We also observed that rugosity migh have an effect on sea cucumber abundance
#Let's use some basic stats to test wether these hypothesis are true

# BASIC ANALYSIS: T-tests and ANOVAs ####
#Testing differences between two groups of data (cateorical variable)

#T-tests####
#How could we determine if abundance at the two depths is different? using a T-test!
#A t-test test whether the true difference in means is equal to 0
plot(cukes$orange_cukes ~ cukes$Depth)
t.test(cukes$orange_cukes ~ cukes$Depth) 
#Shows that these groups are significantly different at Alpha = 0.05!
#T-test output shows variables tested, t value, degrees of freedom, p-value, 
#means of groups, alternative hyp

#Pretty visualization of categorical data (2 levels) with ggplot####
#First, it is necessary to summarize the data. 
#We will use summaryBy() to estimate mean and some error around it, in this case SD
cukes_se <- summaryBy(orange_cukes ~ Depth, cukes, FUN=c(mean,sd,var))
View(cukes_se)

# Error bars represent standard deviation
ggplot(cukes_se, aes(x=Depth, y=orange_cukes.mean, fill=Depth)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=orange_cukes.mean-orange_cukes.sd, ymax=orange_cukes.mean+orange_cukes.sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(y="Sea cucumber abundance") +
  labs(x="Depth") +
  theme_gg()

#ANOVAs####
#What if we want to look at more than two groups?
#Maybe the students collecting the data influenced the results by using different methodology. 
plot(cukes$orange_cukes ~ cukes$Students)

# Data shows like there are some differences for group 1,2? Why? Maybe they were just in the wrong habitat for
# sea cucumbers? 
# We can test if more than 2 groups have different means using an ANOVA.

#For the basic ANOVA function in R we need a balanced number of trials for each group for ANOVA,
summary(cukes)
View(cukes)
#so when you are doing this on your own data make sure that all groups have the same number of measurements
# For our cukes all groups did the same number of quadrats at shallow and deep so sampling is "balanced"
cuke_anova <- aov(cukes$orange_cukes ~ cukes$Students)
summary(cuke_anova)
#Shows there are some significant differences between groups at Alpha = 0.05!
#ANOVA output shows categorical variable tested, degrees of freedom, Sum of Squares, 
#F value, p-value, significance codes

#Once we found differences between groups, we need to know where are those differences
#We use Tukey's posthoc test for that
cuke_posthoc <- TukeyHSD(x=cuke_anova, 'cukes$Students', conf.level=0.95)
cuke_posthoc
#Output gives the differences between groups (means) and their p-values

#Visualization of categorical data (4 levels) with ggplot####
#Let's visualize this nicely
#Again, first, we summarize the data to estimate mean and some error
cukes_sd2 <- summaryBy(orange_cukes ~ Students, cukes, FUN=c(mean,sd,var))
View(cukes_sd2)

# Error bars represent standard deviation
ggplot(cukes_sd2, aes(x=Students, y=orange_cukes.mean, fill=Students)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=orange_cukes.mean-orange_cukes.sd, ymax=orange_cukes.mean+orange_cukes.sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(y="Sea cucumber abundance") +
  labs(x="Depth") +
  theme_gg()


#Linear regression model (continuous variables)####
plot(cukes$rugosity, cukes$orange_cukes) 
#Let's try making a linear model of the relationship between sea cucumber abundance and rugosity
#Remember linear models always take data in the form dependent variable ~ independent variable.

cukemodel <- lm(orange_cukes ~ rugosity, data=cukes) #the basic model in the form of y = a + bx
#After model creation we need to make sure some assumptions are met to validate model
#We look for homocedasticity and normality in residuals 
par(mfrow = c(2,2)) #this tells R to set up a 2x2 grid of 4 plots
plot(cukemodel) #create the diagnostic plots
par(mfrow = c(1,1)) #turn off the 4x4 grid setup or all your future plots will go onto a grid
#the plots look ok, so now let's look at our model summary!
summary(cukemodel) 
#With the lm output you get the estimate for the intercept (a), the slope (b), 
#the standard error for each estimate, and the p-values for those estimates 
#(remember <0.05 is a 'significant' value).
#The R-squared values tells you what proportion of the variation in the data
#is explained by the independent variable(s) (here rugosity)

#Visualization of continous variables with ggplot####
#We can dress up our plot a bit by adding labels color and the model fit
#We do not need to summarize the data in this case but need to predict values based on model for model fit
cukes$predicted1=predict(cukemodel)
View(cukes)
#Plot of pretty plot
ggplot(data=cukes, aes(x=rugosity, y=orange_cukes, color=Depth)) + 
  geom_point() +
# stat_smooth(method = "lm", col = "blue", se = F) +
  geom_line(aes(x = rugosity, y = predicted1)) +
  labs(y="Sea cucumber abundance") +
  labs(x="Rugosity") +
  theme_gg()

#Linear models with different types of predictive variables####
#With linear models we can test both continuous and categorical variables
cukemodel_2 <- lm(orange_cukes ~ Depth + rugosity, data=cukes)
par(mfrow = c(2,2)) #this tells R to set up a 2x2 grid of 4 plots
plot(cukemodel_2) #create the diagnostic plots
#we look for homocedasticity and normality in residual 
par(mfrow = c(1,1)) #turn off the 4x4 grid setup or all your future plots will go onto a grid
#the plots look ok, so now let's look at our model summary!
summary(cukemodel_2) 
#With the lm output you get the estimate for the intercept (a), the slope (b), 
#the standard error for each estimate, and the p-values for those estimates 
#(remember <0.05 is a 'significant' value).
#The R-squared values tells you what proportion of the variation in the data
#is explained by the independent variable(s) (here rugosity and depth)

#Visualization of continous & categorical variables with ggplot####
#We first need to create predictive values based on our model for our model fit 
cukes$predicted2=predict(cukemodel_2)
View(cukes)
#Plot of pretty plot
ggplot(data=cukes, aes(x=rugosity, y=orange_cukes, color=Depth)) + 
  geom_point() +
  #facet_wrap("Depth") +
  #stat_smooth(method = "lm", col = "blue") +
  labs(y="Sea cucumber abundance") +
  labs(x="Rugosity") +
  geom_line(aes(x = rugosity, y = predicted2)) +
  theme_gg()


#LMMs####
#Now let's assume there are small differences in how different groups measured the data that we want to control for, but that we aren't actually interested in. Another example of this is when taking measurements at multiple field sites where conditions vary slightly, and you might want to control for these when getting your  model estimates, but you don't care about the influences of the sites themselves. These nuisance factors are called "random effects" and can be controlled for using a mixed-effects model.

#First we install the mixed model package
install.packages("lme4")
library("lme4")

#Now let's run a mixed model with student group as a random effect and compare with our original model
anemonemodel.2 <- lmer(anemones$reaction.time.s ~ anemones$different.colony + anemones$pool.area.cm2 + (1|anemones$students)) #here +(1|anemone$students) is how you specify a random intercept, that is, if you expect the height of the relationships to vary across sites but not the slope, this is the most basic type of random effect.

#When we compare we see the resulting parameter estimates are only a little different, because we already saw student group doesn't matter, but it can be a lot different!
summary(anemonemodel.2)
summary(anemonemodel)

#---------#--------#---------#--------#
#Now we'll look at a totally different data set to study interactions in models:
# Here we'll look a data on fish age, fish length, and whether it is mature or not
# Variables are age, lengthcm, mature (where 1 = success (mature)), only response variable is categorical
maturedata <-read.csv("C:/Users/Natascia/Desktop/Bamfield/MBE Course/R Workshop/matureWB.csv", header=T, sep=",")
str(maturedata)
head(maturedata)
names(maturedata)
summary(maturedata)
#This time let's attach the data so we don't have to call the whole data set name each time
attach(maturedata)

plot(maturedata)
plot(mature~age)

#Now let's make some models with the variables individually
maturedata.a <- glm(mature~age, family=binomial)
summary(maturedata.a)

maturedata.l <- glm(mature~lengthcm, family=binomial)
summary(maturedata.l)

#But maybe both length and age are important?
maturedata.al <- glm(mature~age+lengthcm, family=binomial)
summary(maturedata.al)

#And we know there is an interaction between age & length, as they grow older, they get longer...
#So we should capture this as an interaction term!
maturedata.alint <- glm(mature~age+lengthcm+age:lengthcm, family=binomial)
summary(maturedata.alint)

#How can we decide which one of these models is best? Using something called the Akaike Information Criterion (AIC). AIC measures the tradeoffs between model fit and support for the variables in the model, with lower values indicating the model that better fits the data. Here we'll use AICc which is adjusted for small sample sizes. You need the package "MuMIn" that we loaded earlier to do this.

AICc(maturedata.a)
AICc(maturedata.l)
AICc(maturedata.al)
AICc(maturedata.alint)
