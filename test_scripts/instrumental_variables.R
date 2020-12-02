# instrumental variables
install.packages("ivreg")
install.packages("AER")
install.packages("lmtest")

#what are the benefits of attaining a certain level of education on income?

data("CollegeDistance")
library(ivreg)
library(AER)
library(lmtest)

# first part - regressing education with distance

#FIRST STAGE
m1 <- lm(education ~ distance, data = CollegeDistance)
summary(m1)
CollegeDistance$ed.pred <- predict(m1)

#SECOND STAGE
m2 <- lm(wage ~ ed.pred, data = CollegeDistance)
summary(m2)

m3 <- ivreg(wage ~ education|distance, data = CollegeDistance)
summary(m3)
?ivreg

## adding covriates

# m4 <- ivreg(wage ~ urban + gender + ethnicity + ...)

## a second exasmple

library(ivreg)
data("SchoolingReturns")
m1 <- lm(log(wage) ~ education, data = SchoolingReturns)
summary(m1)

m2 <- ivreg(log(wage) ~ education|nearcollege4, data = SchoolingReturns)
summary(m2)

m2 <- ivreg(log(wage) ~ education|nearcollege4 +nearcollege2, data = SchoolingReturns)
summary(m2)
?SchoolingReturns
