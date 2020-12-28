
library(readr)
working_data <- read.csv('/cloud/project/data/third_grade_data_cleaned_Grade8.csv')
## is the average attendance score among SC different than
## average attendance score among non SC
# install.packages("ggthemes")
library(ggthemes)
working_data_graph <- working_data %>%
  filter(Demographic.Variable %in% c("SWD", "All Students",
                                     "White", "Black",
                                     "Male", "Female",
                                     "Hispanic", "Poverty",
                                     "Not SWD")) %>%
  group_by(self_contained_option, Demographic.Variable) %>%
  summarise(mean = mean(Percent_Attendance),
            mean_absent = mean(Percent_Chronically_Absent))
working_data_graph <- working_data_graph %>%
  mutate(self_contained_option = ifelse(self_contained_option == 1, "Self-Contained Option",
                                        "No Self-Contained Option"))
ggplot(data = working_data_graph, aes(x = reorder(Demographic.Variable, -mean_absent),
                                      y = mean_absent,
                                      fill = "red")) +
  geom_bar(sta = "identity") +
  facet_wrap(~self_contained_option) +
  xlab ("Demographics") +
  ylab("Percent Chronically Absent") +
  theme_tufte() +
  geom_text(aes(label = round(mean_absent,2), vjust = -.3))

working_data %>%
  group_by(self_contained_option) %>%
  summarise(mean = mean(X..Students.with.Disabilities))