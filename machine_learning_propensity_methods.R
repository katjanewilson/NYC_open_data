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
#shrinkage- there is a possilibity that this can overfit the data
#so shrinkage is a way to constrain vertain parts of the model fitting
# so that it avoids certain chances of overfitting the data, which in some cases
#leads to one observation in a group
# so the defaul in gbm is to specify that, and that is how much deviation you
# are willing to tolerate, shrinkage = 0.01
# this is a way to conrol the effects of over-fitting your model
#stop.method = what criteria do you want to use to detemrine whether an individual who is categorized in one group is doen so precisely
#this is related to balance. How do you know if balance is successful?
# you check to see if covariates are similar
#so in gbm, there are two major stopping method, the first is the effect size, and the second is 
#kolmogoroz Smirnoza. And the 
mod1 <- ps(treat ~age + educ + black+ hispan +
             nodegree + married + re74 + re75, data = lalonde,
           n.trees = 5000,
           interaction.depth = 2,
           shrinkage = 0.01,
           stop.method = c("es.mean", "ks.max"),
           estimand = "ATT",
           verbose = F) #what is the tradeoff in the interaction depth
ba1 <- bal.table(mod1)
ba1

#which stopping 
lalonde$wt <- get.weights(mod1, stop.method = "es.mean")

## the weights for the treatment individual is 1, and the weights ofr the control
# is 1 / propensity score

## but if this is ATE, then the weight for the treatment individual is 1/ estimate propenstiy score

# if you wanted to estimate the treatment effect, what is theeffect of the program on
#average income, then since we have the weights
head(lalonde)

#then, create another object caleld design.ps, and this
#is taken from the survey package, which creates a survey package

#this is referring to whether there is clustering 
design.ps <- svydesign(ids = ~1, weights = lalonde$wt, data = lalonde)

#Estimate ATT
# this will tell us the difference between treatment control invidiausl ,ad than impact on real wages

mod.glm1 <- svyglm(re78 ~ treat, design = design.ps)

summary(mod.glm1)
## so , participating was associated with an increase of 732, but this was not signfiiant
#based on the weighted data

## How to estimate the propensity scores using Random Forest

mod2 <- randomForest(factor(treat) ~ age + educ + black + hispan +
                       nodegree + married + re74 + re75,
                     data = lalonde)
#specify the type as probability because esetiamting the propensity scores
#or you can do the outcome
# and that is if yo uare using random forest o predict the outcome
# if you run it without the [1] then it is the propensity score and find which one is treatment
lalonde$ps.rf <- predict(mod2, newdata = lalonde, type = "prob")[,"1"]
head(lalonde)
#here we have the weights from gbm, and the propensity scores from random forest
#then with this, you are using propensity scores estimated with RF instead of logsitic regression