

######
#working directory
#######

setwd("/cloud/project/scripts")
getwd()

######
#packages
#######
library(readr)
library(tidyverse)


######
# STEP 1
#class size data
#######
X2009_2010_Class_Size_School_level_Detail <- read_csv("../raw/2009_-_2010_Class_Size_-_School-level_Detail.csv")
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
write.csv(Ps043, file = "../data/PS043_class_size.csv")
## just for the Bronx
Ps001 <- bronx_schools %>%
  filter(`SCHOOL CODE` == 'X001')
write.csv(Ps001, file = "../data/PS001_class_size.csv")


######
# STEP 2
#demographics data
#######

library(readr)
X2006_2012_School_Demographics_and_Accountability_Snapshot <- read_csv("/cloud/project/raw/2006_-_2012_School_Demographics_and_Accountability_Snapshot.csv")
bronx_schools_demographics <- X2006_2012_School_Demographics_and_Accountability_Snapshot %>%
  filter(str_detect(`DBN`, "X")) %>%
  mutate(district_75 = ifelse(`DBN` %in% c('07X043',
                                                   '08X304',
                                                   '11X178'), 1, 0))
table(bronx_schools_demographics$district_75)
## just for the Bronx
Ps043_demo <- bronx_schools_demographics %>%
  filter(`DBN` == '07X043')
write.csv(Ps043_demo, file = "../data/PS043_demo.csv")
## just for the Bronx
Ps001_demo <- bronx_schools_demographics %>%
  filter(`DBN` == '07X001')
write.csv(Ps001_demo, file = "../data/PS001_demo.csv")



######
# STEP 3
#attendance data
#######

library(readr)
X2018_2019_Daily_Attendance <- read_csv("../raw/2013-2019_Attendance_Results_-_School.csv")
View(X2018_2019_Daily_Attendance)

bronx_schools_attendance <- X2018_2019_Daily_Attendance %>%
  filter(str_detect(`DBN`, "X")) %>%
  mutate(district_75 = ifelse(`DBN` %in% c('07X043',
                                           '08X304',
                                           '11X178'), 1, 0))
## just for the Bronx
Ps043_attendance <- bronx_schools_attendance %>%
  filter(`DBN` == '07X043')
write.csv(Ps043_attendance, file = "../data/PS043_attendance.csv")
## just for the Bronx
Ps001_attendance <- bronx_schools_attendance %>%
  filter(`DBN` == '07X001')
write.csv(Ps001_attendance, file = "../data/PS001_attendance.csv")


