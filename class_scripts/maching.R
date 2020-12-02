## causal
## import ecsl
library(readr)
ecls <- read_csv("raw/ecls.csv")
View(ecls)
View(ecls)
## install packages
# install.packages("MatchIt")
library(MatchIt)
# install.packages("Zelig") #Zelig allows you to run different models
#along with the propensity score method
#install.packages("tidyverse")
library(tidyverse)

ecls %>%
  group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(c5r2mtsc_std),
            std_error = sd(c5r2mtsc_std)/ sqrt(n_students))

ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')
ecls %>%
  group_by(catholic) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

## so t.test performs the t test. And catholic is between the 
#two groups, and the ecls_ tells R that ew are looking for
#multiple covariates
#this is applyhing a t.test to five variables at the same time
lapply(ecls_cov, function(v){
  t.test(ecls[,v]-ecls[, 'catholic'])
})



### NEAREST NEIGHBOR MATCHING

#create one that only has complete cases
#New Dataset that has complete cases (no missing. data)

#this says, if anyone has missingvalues in any of the 5
#covariates, then remove it from the dataset
ecls_nomiss <- ecls %>%
  select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>%
  na.omit()

## do the matching based on both the caliper AND 
# the euclidean distance

## Nearest Neighbor with a single covariate (w3income)

ecls_nearest<- matchit(catholic ~ w3income, family = "binomial",
                       method = "nearest", caliper = 0.25,
                       data = ecls_nomiss)
summary(ecls_nearest)
plot(ecls_nearest)


## matched. dataset
ecls_nearest_matched <- match.data(ecls_nearest)
##so, this is using the matched dataset, and 
# performing a t test on the income
with(ecls_nearest_matched, t.test(w3income ~ catholic))
#so, the differences in income are not different in the matched
#dataset 

ecls_mahala <- matchit(catholic ~ w3income + race_white +
                         p5hmage + p5numpla + w3momed_hsb, 
                       family = "binomial",
                       method = "nearest",
                       distance = "mahalanobis",
                       caliper = 0.25,
                       data = ecls_nomiss)

summary(ecls_mahala)

# receive the matched dataset

ecls_mahala_matched <- match.data(ecls_mahala)


# consider the case when we have more than five covariates

#propensity score matching

ecls_propensity <- matchit(catholic ~ w3income + race_white +
                             p5hmage + p5numpla + w3momed_hsb,
                           family = "binomial", data = ecls_nomiss)


ecls_propensity_matched <- match.data(ecls_propensity)
head(ecls_propensity_matched)

plot(ecls_propensity)
plot(ecls_propensity,type = "hist")
plot(ecls_propensity,type = "jitter")
