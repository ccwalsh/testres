library(tidyverse)
install.packages(skimr)
library(skimr)

data<-read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

survey <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

survey_IT_au <-survey%>%
  filter(country=="Australia" | country=="australia"|country=="Australi")%>%
  filter(industry=="Computing or Tech" & annual_salary>80000)

survey_IT_au%>%
 count(highest_level_of_education_completed, sort = TRUE)


survey_IT_au%>%
  group_by(gender, city) %>%
 summarise(min_slary=min(annual_salary)) %>%
  arrange(desc(min_slary, ))

hello<-survey_IT_au%>%
  mutate(experience=parse_number(overall_years_of_professional_experience))

highroller <-hello %>%
  mutate(highroller= case_when(annual_salary >100000 ~ "yes",T ~ "no"))
