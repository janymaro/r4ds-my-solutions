{
  library(tidyverse)
}

# Compute the rate for table2, and table4a + table4b. You will need to perform
# four operations: a. Extract the number of TB cases per country per year. b.
# Extract the matching population per country per year. c. Divide cases by
# population, and multiply by 10,000. d. Store back in the appropriate place.
  
table2 %>% 
  filter(type == 'cases') %>% 
  select(count, country, year) %>%
  rename(cases = count) %>% 
  left_join(table2, by = c('country', 'year')) %>% 
  filter(type == 'population') %>% 
  select(country, year, cases, count) %>% 
  rename(population = count) %>% 
  mutate(
    prop = cases / population * 10000
  ) %>% 
  select(country, year, prop) %>% 
  left_join(table2, by = c('country', 'year'))

# Exercise 2 - table4a + table4b

table4a %>% 
  rename(cases_1999 = `1999`, cases_2000 = `2000`) %>% 
  left_join(table4b, by = 'country') %>% 
  mutate(
    prop_1999 = cases_1999 / `1999` * 10000,
    prop_2000 = cases_2000 / `2000` * 10000
  )

# Exercise 3
# Re-create the plot showing change in cases over time using table2 instead of
# table1. What do you need to do first?

table2 %>% 
  filter(type == 'cases') %>% 
  transmute(
    country,
    year,
    cases = count
  ) %>% 
  ggplot(aes(year, cases)) +
  geom_line(aes(group = country), color = 'gray50') +
  geom_point(aes(color = country))


