### 
##PART 1 MATCHING with Propensity Score
###


##small sample matching

library(readr)

library(pacman)
p_load(MatchIt, dplyr, survey, tableone, twang, ipw, ggplot2)
working_data <- read.csv('/cloud/project/data/third_grade_data_cleaned.csv')
working_data <- working_data %>%
  filter(Demographic.Variable == "SWD")


working_data %>%
  group_by(self_contained_option) %>%
  summarise(mean_attendance = mean(Percent_Attendance),
            mean_chronic_absent = mean(Percent_Chronically_Absent))
with(working_data, t.test(Percent_Attendance ~ self_contained_option))
with(working_data, t.test(Percent_Chronically_Absent ~ self_contained_option))
table(working_data$self_contained_option)
## the t test is statistically signifiant, but that
# is done without any matching

# find the diffrences on the covariates
school_covariates <- c('X..Poverty', 'X..Black', 'X..Male', 'Economic.Need.Index')
working_data %>%
  group_by(self_contained_option) %>%
  select(one_of(school_covariates)) %>%
  summarise_all(funs(mean(., na.rm=T)))
# do a t.test to find if there are statistically different differences

lapply(school_covariates, function(v) {
  t.test(working_data[, v] ~ working_data[ ,'self_contained_option'])
})

# set seed
set.seed(1731)

## issue with the matching, so what if we switch treatment and control
working_data$treatment_indicator <- ifelse(working_data$self_contained_option == 1, 0, 1)
working_data$new_outcome_labelled <- ifelse(working_data$treatment_indicator == 1, "no SC", "SC")
library(MatchIt)
school_nearest <- matchit(formula = treatment_indicator ~ Economic.Need.Index +
                            X..Black + X..Male + X..Poverty, data = working_data,
                          method = "nearest",
                          family = "binomial",
                          caliper = 0.25)
summary(school_nearest)
table(working_data$treatment_indicator)
plot(school_nearest)


#create the matched set
nearest_matched <- match.data(school_nearest)

#350 schools were matched
dim(nearest_matched)

## now look at the means of the covariates
#matching was successful because the poverty rates are around .72 now together
nearest_matched %>%
  group_by(self_contained_option) %>%
  select(X..Poverty) %>%
  summarise_all(funs(mean))

## also can conduct a t test to assess the matches
with(nearest_matched, t.test(Percent_Chronically_Absent ~self_contained_option))

### ## estimating treatment effects

model <- lm(Percent_Chronically_Absent ~ treatment_indicator, data = nearest_matched)
summary(model)
table(nearest_matched$treatment_indicator)
# if self contained is 1 (treatment), you have it, you have higher attenance








### 
##PART 2 IPTW Weighting
###



## IPTW
#add the propensity scores
working_data$ps <- school_nearest$distance
#estimate the effect of SC option using IPTW
#create IPTW weights - 0 is no SC, which is the treatment
working_data$iptw <- ifelse(working_data$new_outcome_labelled == 'no SC', 1/(working_data$ps),
                            1/(1-working_data$ps))
#stabilized weights
working_data$stable.iptw <- ifelse(working_data$new_outcome_labelled == 'no SC',
                                   (mean(working_data$ps[working_data$new_outcome_labelled == 'no SC'])/working_data$ps),
                                   (mean(1-working_data$ps[working_data$new_outcome_labelled == 'SC'])/(1-working_data$ps)))
summary(ecls_nomiss$stable.iptw)
working_data_nomiss<- working_data %>%
  select(Percent_Chronically_Absent, treatment_indicator, X..Poverty, X..Male, X..Black, Economic.Need.Index, ps, iptw, stable.iptw,
         treatment_indicator)
#weighted data - create a weighted version of the data 
working_data_weighted <- svydesign(ids = ~1, data = working_data_nomiss, weights = working_data_nomiss$iptw)
#check the balance
SC_iptw_table <- svyCreateTableOne(vars = school_covariates, strata = "treatment_indicator", data = working_data_weighted,
                                   test = F)
SC_iptw_table
print(SC_iptw_table, smd=T)
boxplot(working_data_nomiss$X..Black ~ working_data_nomiss$treatment_indicator)
### check the covariate distribution for all the weights

for(i in 1:nrow(working_data_nomiss)){
  working_data_nomiss$X..Black[i] <- ifelse(working_data_nomiss$treatment_indicator == 1,
                                            (working_data_nomiss$X..Black[i]*(mean(working_data_nomiss$ps[working_data_nomiss$treatment_indicator==1])))/working_data_nomiss$ps[i],
                                            (mean(1-(working_data_nomiss$ps[working_data_nomiss$treatment_indicator ==0]))*working_data_nomiss$X..Black[i])/(1-working_data_nomiss$ps[i]))
}
boxplot(working_data_nomiss$X..Black ~working_data_nomiss$new_outcome)


# plot the weights
ipwplot(working_data_nomiss$ps, logscale = F,
        main = "propensity scores")
#lastly, test if distributions of covaraites are similar/diffeernt before or after weighting
ks.test(working_data_nomiss$X..Black[working_data_nomiss$treatment_indicator == 1],
        working_data_nomiss$X..Black[working_data_nomiss$treatment_indicator == 0])

#estimate the ate
mod_out_iptw <- lm(Percent_Chronically_Absent ~ treatment_indicator, weights = working_data_nomiss$iptw, 
                   data = working_data_nomiss)
summary(mod_out_iptw)
## still signifiant
table(working_data_nomiss$treatment_indicatory)







### 
##PART 3: Subclassification
###

mod2 <- matchit(formula = treatment_indicator ~ Economic.Need.Index +
                  X..Black + X..Male + X..Poverty, data = working_data_nomiss,
                method = "subclass", subclass = 5)
wd_nomiss2 <- data.frame(cbind(working_data_nomiss, match.data(mod2)[,c("distance", "subclass")]))                
head(wd_nomiss2)

## check out subclasses
wd_nomiss2$subclass <- as.factor(wd_nomiss2$subclass)
wd_nomiss2$X..Poverty <- as.factor(wd_nomiss2$X..Poverty)
wd_nomiss2 %>%
  group_by(subclass, treatment_indicator) %>%
  summarise(mean_ps = mean(ps),
            mean_economic_need = mean(Economic.Need.Index),
            mean_black = mean(X..Black))
## so, all students in subclass 3 have similar propensity scores, etc.
table(wd_nomiss2$self_contained_binary)
dat <- wd_nomiss2[,c("distance", "treatment_indicator", "subclass")]
dat$Observations <- rep("NoSC", length(wd_nomiss2$treatment_indicator))
dat$Observations[dat$treatment_indicator == 0] <- "SC"
dat$ymax <- 1
quant <- quantile(wd_nomiss2$distance, probs = seq(0,1,1/5))
q <- data.frame(id = names(quant), values = unname(quant), stringsAsFactors = FALSE)
pp <- ggplot(data = dat, aes(x = distance, group = Observations))
pp + geom_density(aes(x = distance, linetype = Observations), size = 0.75, data = dat)+
  xlab("Propensity Score Logit") +
  ylab("Density") +
  geom_vline(xintercept = quant[(2:5)], linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "bottom")


##estimate the ATE

mod_out_sub <- lm(Percent_Chronically_Absent ~ treatment_indicator +factor(subclass) + factor(subclass) *treatment_indicator -1, 
                  data = wd_nomiss2)
summary(mod_out_sub)

