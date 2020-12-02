######
# STEP 1
#load PS43 and PS 001 data
#######

PS001_attendance <- read.csv("../data/PS001_attendance.csv")
PS001_demo <- read.csv("../data/PS001_demo.csv")
PS001_class_size <- read.csv("../data/PS001_class_size.csv")

PS043_attendance <- read.csv("../data/PS043_attendance.csv")
PS043_demo <- read.csv("../data/PS043_demo.csv")
PS043_class_size <- read.csv("../data/PS043_class_size.csv")


######
# STEP 2
#some summary differences in schools
#######


PS001_demo %>%
  group_by(schoolyear) %>%
  summarise(black_per, hispanic_per, white_per)
PS043_demo %>%
  group_by(schoolyear) %>%
  summarise(black_per, hispanic_per, white_per)


PS001_class_size %>%
  filter(PROGRAM.TYPE == "SPEC ED" | PROGRAM.TYPE == "GEN ED") %>%
  group_by(GRADE, PROGRAM.TYPE) %>%
  summarise(AVERAGE.CLASS.SIZE)
PS043_class_size %>%
  filter(PROGRAM.TYPE == "SPEC ED" | PROGRAM.TYPE == "GEN ED") %>%
  group_by(GRADE, PROGRAM.TYPE) %>%
  summarise(AVERAGE.CLASS.SIZE)


#attendance of only the SWD (studens with disabilities)
class(PS001_attendance$X..Days.Absent)
PS001_attendance %>%
  filter(Demographic.Variable == "SWD", Grade==3) %>%
  filter(X..Attendance != 's') %>%
  mutate(days_absent = as.numeric(X..Attendance)) %>%
  group_by(Year) %>%
  mutate(avg_days_absent = mean(days_absent)) %>%
  select(Grade, Year, avg_days_absent)

PS043_attendance %>%
  filter(Demographic.Variable == "SWD", Grade==3) %>%
  filter(X..Attendance != 's') %>%
  mutate(days_absent = as.numeric(X..Attendance)) %>%
  group_by(Year) %>%
  mutate(avg_days_absent = mean(days_absent)) %>%
  select(Grade, avg_days_absent)


#attendance of only the SWD (studens with disabilities)
class(PS001_attendance$X..Days.Absent)
PS001_attendance %>%
  filter(Demographic.Variable == "SWD") %>%
  filter(X..Attendance != 's') %>%
  mutate(days_absent = as.numeric(X..Attendance)) %>%
  group_by(Grade) %>%
  summarise(avg_days_absent = mean(days_absent))

PS043_attendance %>%
  filter(Demographic.Variable == "SWD") %>%
  filter(X..Attendance != 's') %>%
  mutate(days_absent = as.numeric(X..Attendance)) %>%
  group_by(Grade) %>%
  summarise(avg_days_absent = mean(days_absent))
