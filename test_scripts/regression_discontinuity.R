# regression discontinuity

install.packages("rdd")
install.packages("rddtools")
install.packages("rdrobust")

library(rdd)
library(rddtools)
library(rdrobust)

#simulated example
set.seed(8)

# this is sampling random observations from a distribution
#this gives 1000 values that range between -1 and 1
x <- runif(1000, -1,1) #rating/score
#using 0 as the cutoff, and people whose values are
#above 0 are treatment, and less 0 are control
y <- 2 +3*x + 10*(x >=0) +rnorm(10000) #outcome is 2 times the rating
y
View(cbind(y,x))


## now, estimate the treatment effect about that cut-point, which is 0

#this is inputting outcome, rating, and telling R what the cutpoint is
data <- rdd_data(y,x,cutpoint = 0)

plot(data, col = "red", cex = 0.35, xlab = "Rating", ylab = "Outcome")


## analyze the data to get an estimate of the treatment effect


## SHARP

rdd_mod1 <- rdd_reg_lm(rdd_object = data, slope = "same") #constant treatment effect
summary(rdd_mod1)

rdd_mod1


## if you believe that the slopes are different
#or some suggestion of tretment heterogeneity

#what this does is essentially include an interaction effect
rdd_mod1 <- rdd_reg_lm(rdd_object = data, slope = "separate")
summary(rdd_mod1)


## can include polynomial terms
#polynomial 2
rdd_mod3 <- rdd_reg_lm(rdd_object = data, slope = "same", order = 2)
summary(rdd_mod3)


#kernal estimation and bandwith

bw <- with(data, IKbandwidth(x, y , cutpoint = 0))
rdd_mod4 <- RDestimate(y ~x, cutpoint = 0, data = data, bw = bw)
summary(rdd_mod4)

#Type 1 Fuzzy
#if it is less than .6, then that person receives the treatment
data$treatment1 <- (runif(nrow(data)) < 0.6)*(data$x >=0)

#this treatment 1 variable indiciates if a person
#is assigned to treatment actually gets it

head(data)

rdd_mod5 <- RDestimate(y ~x + treatment1, data = data)
summary(rdd_mod5)


## type 2 - you have both no shows and crossovers

data$treament2 <-
