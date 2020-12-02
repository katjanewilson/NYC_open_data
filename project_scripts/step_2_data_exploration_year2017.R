######
# STEP 1
#load 2017-2018 third grade data
#######
library(readr)
library(tidyverse)
setwd("/cloud/project/project_scripts")
third_grade_data <- read.csv("../data/third_grade_data_2017.csv")

#boroughs
third_grade_data <- third_grade_data %>%
  mutate(borough = case_when(str_detect(`DBN`, "X") ~"Bronx",
                             str_detect(`DBN`, "Q") ~ "Queens",
                             str_detect(`DBN`, "K") ~"Brooklyn",
                             str_detect(`DBN`, "M") ~ "Manhattan",
                             str_detect(`DBN`, "R") ~"Staten Island"))
table(third_grade_data$borough)



######
# STEP 2
#mutate into whether or not it has a self-contained option
#######
## check distinct
#768 unique schools
length(unique(third_grade_data$DBN))
third_grade_data <- third_grade_data %>%
  mutate(GT = case_when(is.na(G.T) ~ 0),
         GenEd = case_when(is.na(Gen.Ed) ~ 0),
         GT = case_when(is.na(G.T) ~ 0),
         ICT_2 = case_when(is.na(ICT) ~ 0),
         SC_12.1 = case_when(is.na(SC.12.1) ~ 0),
         SC_12.1.1 = case_when(is.na(SC.12.1.1) ~ 0),
         SC_15.1 = case_when(is.na(SC.15.1) ~ 0),
         SC_6.1.1 = case_when(is.na(SC.6.1.1) ~ 0),
         SC_8.1.1 = case_when(is.na(SC.8.1.1) ~ 0))
                        
third_grade_data <- third_grade_data %>%
  mutate(GT= ifelse(is.na(GT), 1, 0),
         GenEd= ifelse(is.na(GenEd), 1, 0),
         ICT_2= ifelse(is.na(ICT_2), 1, 0),
         SC_12.1= ifelse(is.na(SC_12.1), 1, 0),
         SC_12.1.1= ifelse(is.na(SC_12.1.1), 1, 0),
         SC_15.1= ifelse(is.na(SC_15.1), 1, 0),
         SC_6.1.1= ifelse(is.na(SC_6.1.1), 1, 0),
         SC_8.1.1= ifelse(is.na(SC_8.1.1), 1, 0))

third_grade_data <- third_grade_data %>%
  mutate(self_contained = ifelse(SC.12.1 == 1 | SC_12.1.1 == 1 |
                                   SC.15.1 ==1 | SC_6.1.1 ==1 | SC_8.1.1==1 , 
         1, 0))
third_grade_data <- third_grade_data %>%
  mutate(self_contained_option = ifelse(is.na(self_contained), 0, 1))
third_grade_data <- third_grade_data %>%
  mutate(gifted_talented_option = ifelse(GT == 1, 
                                 1, 0))
third_grade_data <- third_grade_data %>%
  mutate(ICT_option = ifelse(ICT_2 == 1, 
                                  1, 0))
third_grade_data <- third_grade_data %>%
  mutate(self_contained_and_GT = ifelse(gifted_talented_option == 1 &
                                          self_contained_option == 1, 
                             1, 0))
third_grade_data <- third_grade_data %>%
  mutate(self_contained_and_GT_and_ICT = ifelse(gifted_talented_option == 1 &
                                          self_contained_option == 1 &
                                            ICT_option == 1, 
                                        1, 0))
third_grade_data <- third_grade_data %>%
  mutate(only_gen_ed = ifelse(gifted_talented_option == 0 &
                                                  self_contained_option == 0 &
                                                  ICT_option == 0 &
                                GenEd == 1, 
                                                1, 0))
write.csv(third_grade_data, '/cloud/project/data/third_grade_data_cleaned.csv')
######
# STEP 3
#find numbers/sample size
#######
##527 schools have a self contained option, 221 don't
third_grade_data %>%
  group_by(self_contained_option) %>%
  summarise(length(unique(DBN)))
## only 84 schools have a GT option
third_grade_data %>%
  group_by(gifted_talented_option) %>%
  summarise(length(unique(DBN)))
## 696 have a GenEd option for third grade
## the ones that don't have only ICT
third_grade_data %>%
  group_by(only_gen_ed) %>%
  summarise(length(unique(DBN)))

working_data <- third_grade_data %>%
  select(DBN, Percent_Attendance, Demographic.Variable, X..Poverty, borough,
         self_contained_option, gifted_talented_option)

## for each borough
ggplot(data = third_grade_data) +
  geom_histogram(mapping = aes(x = Economic.Need.Index)) +
  facet_wrap(~borough)
ggplot(data = third_grade_data) +
  geom_histogram(mapping = aes(x = X..Black)) +
  facet_wrap(~borough)
# for whether or not it has a self-contained option
ggplot(data = third_grade_data) +
  geom_histogram(mapping = aes(x = X..Black)) +
  facet_wrap(~self_contained_option)
ggplot(data = third_grade_data) +
  geom_histogram(mapping = aes(x = X..Black)) +
  facet_wrap(~gifted_talented_option)
ggplot(data = third_grade_data) +
  geom_histogram(mapping = aes(x = X..Poverty)) +
  facet_wrap(~self_contained_option)
ggplot(data = third_grade_data) +
  geom_histogram(mapping = aes(x = X..Poverty)) +
  facet_wrap(~gifted_talented_option)

######
# STEP 3
#percent attendance for those with a self-contained option
#######
working_data <- working_data %>%
  filter(Percent_Attendance != 's')
working_data$Percent_Attendance <- as.numeric(working_data$Percent_Attendance)
attendance_compare <- working_data %>%
  group_by(self_contained_option, Demographic.Variable) %>%
  summarise(mean_attendance = mean(Percent_Attendance))
working_data %>%
  group_by(gifted_talented_option) %>%
  summarise(mean_attendance = mean(Percent_Attendance))
