library(tidyverse)
library(lubridate)
cleandatset<- survey %>%
  
  mutate(timestamp = mdy_hms(timestamp),
         
         age_category = fct_relevel(fct_reorder(how_old_are_you, parse_number(how_old_are_you)), "under 18"),
         
         experience_overall = str_replace(overall_years_of_professional_experience, " - ", "-"),
         
         experience_overall = fct_reorder(experience_overall, parse_number(experience_overall)),
         
         experience_in_field = str_replace(years_of_experience_in_field, " - ", "-"),
         
         experience_in_field = fct_reorder(experience_in_field, parse_number(experience_in_field))) %>%
  
  mutate(gender = fct_collapse(coalesce(gender, "Other or prefer not to answer"), "Other or prefer not to answer" = c("Other or prefer not to answer", "Prefer not to answer")),
         
         race = fct_lump(coalesce(race, "Other"), 4),
         
         age=parse_number(how_old_are_you))



survey_usd <- survey %>%
  
  filter(currency == "USD") %>%
  
  filter(annual_salary >= 5000,
         
         annual_salary <= 2e6) %>%
  
  mutate(state = str_remove(state, ", .*"))


survey_usd %>% ggplot(
  aes( x = age,
       y = annual_salary,
       colour = gender)
)+ geom_point()+
  facet_wrap(survey_usd$experience_in_field)
