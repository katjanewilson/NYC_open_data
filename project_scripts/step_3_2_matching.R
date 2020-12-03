
library(readr)
working_data <- read.csv('/cloud/project/data/third_grade_data_cleaned.csv')
## is the average attendance score among SC different than
## average attendance score among non SC
library(scales)

working_data_graph <- working_data %>%
  filter(Demographic.Variable %in% c("SWD", "All Students",
                                     "White", "Black",
                                     "Male", "Female",
                                     "Hispanic", "Poverty")) %>%
  group_by(self_contained_option, Demographic.Variable) %>%
  summarise(mean = mean(Percent_Attendance),
            mean_absent = mean(Percent_Chronically_Absent))
## set the levels in order we want
levels(working_data_graph$Demographic.Variable)
working_data_graph %>%
  count(Position) %>%
  mutate(Pso)
ggplot(data = working_data_graph, aes(x = reorder(Demographic.Variable, -mean_absent),
                                y = mean_absent)) +
         geom_bar(sta = "identity") +
  facet_wrap(~self_contained_option)
