#import data


#### CLASS SIZE
X2009_2010_Class_Size_School_level_Detail <- read_csv("2009_-_2010_Class_Size_-_School-level_Detail.csv")
View(X2009_2010_Class_Size_School_level_Detail)
library(readr)
X2006_2012_School_Demographics_and_Accountability_Snapshot <- read_csv("2006_-_2012_School_Demographics_and_Accountability_Snapshot.csv")
View(X2006_2012_School_Demographics_and_Accountability_Snapshot)

library(tidyverse)
## just for the Bronx
bronx_schools <- X2009_2010_Class_Size_School_level_Detail %>%
  filter(str_detect(`SCHOOL CODE`, "X")) %>%
  mutate(district_75 = ifelse(`SCHOOL CODE` %in% c('X043',
                                                      'X304',
                                                      'X178',
                                                      'X034',
                                                      'X006',
                                                      'X338'), 1, 0))
table(bronx_schools$district_75)
## just for the Bronx
Ps043 <- bronx_schools %>%
  filter(`SCHOOL CODE` == 'X043')
names(Ps043)
## just for the Bronx
Ps001 <- bronx_schools %>%
  filter(`SCHOOL CODE` == 'X001')
names(Ps043)


#### DEMOGRAPHICS
bronx_schools_demographics <- X2006_2012_School_Demographics_and_Accountability_Snapshot %>%
  filter(str_detect(`DBN`, "X")) %>%
  mutate(district_75 = ifelse(`DBN` %in% c('07X043',
                                                   '08X304',
                                                   '11X178'), 1, 0))
table(bronx_schools_demographics$district_75)
## just for the Bronx
Ps043_demo <- bronx_schools_demographics %>%
  filter(`DBN` == '07X043')
names(Ps043)
## just for the Bronx
Ps001_demo <- bronx_schools_demographics %>%
  filter(`DBN` == '07X001')
names(Ps043)
Ps001_demo %>%
 group_by(schoolyear) %>%
  summarise(black_per, hispanic_per, white_per)
Ps043_demo %>%
  group_by(schoolyear) %>%
  summarise(black_per, hispanic_per, white_per)



### Attendance

library(readr)
X2018_2019_Daily_Attendance <- read_csv("2013-2019_Attendance_Results_-_School.csv")
View(X2018_2019_Daily_Attendance)

bronx_schools_attendance <- X2018_2019_Daily_Attendance %>%
  filter(str_detect(`DBN`, "X")) %>%
  mutate(district_75 = ifelse(`DBN` %in% c('07X043',
                                           '08X304',
                                           '11X178'), 1, 0))
## just for the Bronx
Ps043_attendance <- bronx_schools_attendance %>%
  filter(`DBN` == '07X043')
names(Ps043)
## just for the Bronx
Ps001_attendance <- bronx_schools_attendance %>%
  filter(`DBN` == '07X001')
names(Ps043)


### save current files

