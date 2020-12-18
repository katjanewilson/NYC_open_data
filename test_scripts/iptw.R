#
install.packages("pacman")
library(pacman)
p_load(MatchIt, dplyr, survey, tableone, twang, ipw, ggplot2)
ecls <- read_csv("/cloud/project/raw/ecls.csv")

ecls_cov <- c("race_white", "p5hmage", "w3income", "p5numpla",
              "w3momed_hsb")

##remove cases with missing data
ecls_nomiss <- ecls %>%
  select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>%
  na.omit()
mod <- matchit(catholic ~ race_white + p5hmage + w3income +
                 p5numpla + w3momed_hsb,
               data = ecls_nomiss)
ecls_nomiss$ps <- mod$distance
head(ecls_nomiss)
## summary of hte PS to see their range
summary(ecls_nomiss$ps)
# so ,we have some individuals with very small propensity scores,
#with some close to 0, and keep an eye out because with IPTW
#they will get large weights

## so now to use IPTW, to create the weights, they wil lbe
#1 over hte propensity score for hose ewho attended cath;olic
#and 1/(1-propensity scorE)for those who are in public school

#create the weights
ecls_nomiss$iptw <- ifelse(ecls_nomiss$catholic == 1, 1/(ecls_nomiss$ps),
                           1/(1-ecls_nomiss$ps))
head(ecls_nomiss)
summary(ecls_nomiss$iptw)
## so, this is illustration is that you might want to trim the sample
## see that there are large iptw weights for some of them

# so you have he weighs created for the propenskty scores

#what if you want to minimize the individuals with very large weight,syou can 
#also stabliize them
# this is taking the invidiaus. iptw weight, and mulitplying it by 
# the function of the scores across treatment group

#stabilized weights
#this is just modifying the weights by multiplying by the average
#in the numerators
#this bracketing indicates that the average is only for catholic school students, or for public school students
ecls_nomiss$stable.iptw <- ifelse(ecls_nomiss$catholic == 1,
                                  (mean(ecls_nomiss$ps[ecls_nomiss$catholic ==1])/ecls_nomiss$ps),
                                  (mean(1-ecls_nomiss$ps[ecls_nomiss$catholic==0])/(1-ecls_nomiss$ps)))
summary(ecls_nomiss$stable.iptw)

#check whether weighting is helpful in terms of balancing the two groups, the cathlic and public school
# create a weighted version of the data

#weighted data

#combines the information on the original with the weights on the others
#this is used in surveying
#since we are not using clustering, then do ~1
ecls_nomiss_weighted <- svydesign(ids = ~1, data = ecls_nomiss, weights = ecls_nomiss$iptw)
#so this creates a weighted form of the dataset

#Balance table
# use the package called table 1
ecls_table <- svyCreateTableOne(vars = ecls_cov, strata = "catholic", data = ecls_nomiss_weighted,
                                test = F)
ecls_table
#this will print the standardized mean difference as well
#these five variables that are used in the propensity score stimation
#for now, ignore the nubmers in the parentheses, because these are weighted, and
#also ignore the first row, because that is a weighted version of the sample size
#isntead, focus on the last column, SMD, which is the standardized mean difference of ea h covariate
#using the inverse probability weights
#you see that most of the differences are pretty small
print(ecls_table, smd=T)
#, so if you use he rule of a teneth of a standardd deviation, then balancing seems pretty successful.
# these are the values that are used in papers. They will provide the standardized mean dfifference in this form

#Box plot
#this creates a fifutre for each of the covariates that we are using in the estimation
#start with a boxplot of the unweighted data

boxplot(ecls_nomiss$w3income ~ ecls_nomiss$catholic)
## this does this just quickly be seperating the boxplots

##so, there are differences if you look at the 
#average or median income between public and catholic students
#and differen es in the median income

#if wanted to compare the weighted data, then 
# we can take each students income data and weight it using IPTW or the stable version of it
# just tto make sure that the correct weight is applied
# use the four loop to appply a weighted version to each version

for(i in 1:nrow(ecls_nomiss)){
ecls_nomiss$w3income2[i] <- ifelse(ecls_nomiss$catholic == 1,
                                  (ecls_nomiss$w3income[i]*(mean(ecls_nomiss$ps[ecls_nomiss$catholic==1])))/ecls_nomiss$ps[i],
                                  (mean(1-(ecls_nomiss$ps[ecls_nomiss$catholic ==0]))*ecls_nomiss$w3income[i])/(1-ecls_nomiss$ps[i]))
}
boxplot(ecls_nomiss$w3income2 ~ ecls_nomiss$catholic)

## what this is showing is that, comparing the previous one (the unweighted),
# to the weighted, there does seems to be some slight improvement from using weighting


## plot of the IPTW weights
ipwplot(ecls_nomiss$ps, logscale = F,
        main = "propensity scores")
# these show what the IPW weights are just from the propensity scores
# there is a bit of skewness
# some observations are larger, that are closer to 0.5
# others are smaller, closer to 0
#this is a simple plot that we can use to assess if there is weight trimming
# the last diagnostic is to test formally whether
#the distributions of covariaates are similar before or after
#weighting
# so we can use the Kolmogorov-Smirnov test

ks.test(ecls_nomiss$w3income2[ecls_nomiss$catholic == 1],
        ecls_nomiss$w3income2[ecls_nomiss$catholic == 0])
## so the output of this test is given here
## we see that the test staistic is given by the d variable 
#which stands for distance
#and the p value is very small, it stands for zero
#so even after weighting, the income 2 variable is still
#diffrent between the two groups
#if you change the size of the window, and go back to original plots
#then you notice, after weighting, there seems to be
#a lot more outliers compared to before. tHe public school
#students have more outliers. Although they look similar in the 
#boxplots, weighting might habe exacerbaed some of the observations
#so the differences in the two groups might be
#dirving the difference sin the test

#nevertheless, if the income variable is improtant,
#then you should still include it in the analysis, because
#it is a variable that can potentiallly impact,
#and we know that there are differences in the two groups beforehand


#estimate the ATE
mod_out_iptw <- lm(c5r2mtsc_std ~ catholic, weights = ecls_nomiss$iptw, 
                   data = ecls_nomiss)
#so this is a linear model of the average mathe score against other variable
# and we wil lget the stimate of the weights

summary(mod_out_iptw)
## so we see that the effect of attending catholic school is small,
#but it is not statistically signifiant
#remember, attending catholict school was a positive effect in the beginning,
# but now since we have matched, it no longer is

## what about including covaraites in the outcome model?
# you can maybe include some covariates in the outcome model that
#were not so ewl lbalanced, and the purpose of this is to control for the effect
#of this variable. Because imagine if income has an impact on match score


#Subclassification

# the idea is to take all the scores, divide them, and divide so tha the
#propensity scores for each group are more similar than other groups

#previous we were using full, but here we are using subclass
mod2 <- matchit(catholic ~ race_white + p5hmage + w3income+
                  p5numpla + w3momed_hsb, method = "subclass", subclass = 5,
                data = ecls_nomiss)
ecls_nomiss2 <- data.frame(cbind(ecls_nomiss, match.data(mod2)[,c("distance", "subclass")]))                
head(ecls_nomiss2)
## so, all the students who belong to subclass 3, ahve similar propensity scores,
#and all the students in subclass 4 have similar propensit yscores
#this way of goruping gives yo ua sense of the homogeneity in each group

#create plots to illustrate what the subclasses look lik

#plot for subclassification

## just redo the plot that she has


### estimate the ATE

mod_out_sub <- lm(c5r2mtsc_std ~ catholic +factor(subclass) +
                    factor(subclass)*catholic - 1, data = ecls_nomiss2)
summary(mod_out_sub)     
