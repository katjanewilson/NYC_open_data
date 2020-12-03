library(pacman)
p_load(MatchIt, dplyr, survey, tableone, twang, ipw, ggplot2, randomForest)
data(lalonde)

#set seed for reproducability of algorithm
set.seed(8)
#re74 are ages in respective years
head(lalonde)

#goal is to match the people who got into the program with those who did not
#but propensity scores using machine learning methods

#start by matching on 8 covariates

#Estimate propensity scores using gbm(generalized boosting)
#each tree represents a way of estimating the propensity score using the dataset
# the more trees, the more precision
#but this can take time, because going through it, there are more
# if you are running gbm with a larger dataset, it might take longer
#this dataset is not very big, it only has 614 observations
#interaction.depth = this is important if you beleive that not only the original
# covariates, but also the interactions of the covariates should be included
#so the interaction.depth tells you how many you should include in the model
# set it to 1 if no interaction, but set it to >1 if you think that age * education
# is important, specifys all individual covariates, and all of their 2
#way interactions
mod1 <- ps(treat ~age + educ + black+ hispan +
             nodegree + married + re74 + re75, data = lalonde,
           n.trees = 5000,
           interaction.depth = 2) #what is the tradeoff in the interaction depth


