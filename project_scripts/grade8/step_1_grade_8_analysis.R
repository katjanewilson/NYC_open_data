
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
library(readr)
X2017_2018_SCHOOL_LEVEL_CLASS_SIZE_REPORT <- read_csv("/cloud/project/raw/2017-2018_SCHOOL_LEVEL_CLASS_SIZE_REPORT.csv")
#take out class size because that doesn't include D75
X2018_2019_School_Demographic_Snapshot <- read_csv("/cloud/project/raw/2018-2019_School_Demographic_Snapshot.csv")
X2013_2019_Attendance_Results_School <- read_csv("/cloud/project/raw/2013-2019_Attendance_Results_-_School.csv")

table(X2013_2019_Attendance_Results_School$`Demographic Category`)

##make a dataset of just grade 3 for school year 2017-2018
Attendance_2017 <- X2013_2019_Attendance_Results_School %>%
  filter(Grade == 8) %>%
  filter(Year == '2017-18') %>%
  rename('Percent_Attendance' = '% Attendance',
         'Percent_Chronically_Absent' = '% Chronically Absent') %>%
  select(DBN, `School Name`, Year, Percent_Attendance, `Demographic Category`, `Demographic Variable`, Percent_Chronically_Absent)
Demographics_2017 <- X2018_2019_School_Demographic_Snapshot %>%
  filter(Year == '2017-18') %>%
  select(DBN, `School Name`, Year, `Total Enrollment`, `Grade 8`,'% Female', '% Male', '% Black', '% White', '% Students with Disabilities', '% Poverty', 'Economic Need Index')
ClassSize_2017 <- X2017_2018_SCHOOL_LEVEL_CLASS_SIZE_REPORT %>%
  filter(`Grade Level` == 8 | `Grade Level` == 'K-8 SC') %>%
  group_by(DBN, `Program Type`) %>%
  summarise(avg_class = mean(`Average Class Size`))
ClassSize_2017_spread <- spread(ClassSize_2017, `Program Type`, avg_class)
ClassSize_2017_spread <- ClassSize_2017_spread %>%
  rename("Gen-Ed Class Size" = '3',
         "Sped Class Size" = 'K-8 SC')

## merge

merged <- merge(Attendance_2017, Demographics_2017, by = 'DBN')
merged <- merge(merged, ClassSize_2017_spread, by = 'DBN')
third_grade_data_2017 <- merged
write.csv(third_grade_data_2017, file = "/cloud/project/data/third_grade_data_2017_Grade8.csv")



sample <- X2017_2018_SCHOOL_LEVEL_CLASS_SIZE_REPORT %>%
  filter(`Grade Level` == "8" | `Grade Level` ==  "K-8 SC") %>%
  filter(DBN == "01M019" | DBN == "01M034" )
sample2 <- Attendance_2017 %>%
  filter(`Demographic Variable` == "SWD" | `Demographic Variable` ==  "Not SWD") %>%
  filter(DBN == "01M019" | DBN == "01M034" )




