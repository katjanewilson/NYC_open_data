### small sample matching

library(readr)
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

# do a simple match for this

## issue with the matching, so what if we switch treatment and control
working_data$new_outcome <- ifelse(working_data$self_contained_option == 1, 0, 1)
library(MatchIt)
school_nearest <- matchit(formula = new_outcome ~ X..Poverty +
                            X..Black + X..Male + Economic.Need.Index, data = working_data,
        method = "nearest",
        family = "binomial",
        caliper = 0.25)
summary(school_nearest)
table(working_data$new_outcome)
plot(school_nearest)

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
with(nearest_matched, t.test(Percent_Attendance ~self_contained_option))

###### MAHAL

## add mahalanobis matching here

########


### PROPENSITY SCORE


## add propensity score here



### ## estimating treatment effects

model <- lm(Percent_Attendance ~ self_contained_option, data = nearest_matched)
summary(model)
# if self contained is 1, you have it, you have lower attendance

### subclassification - add that here

