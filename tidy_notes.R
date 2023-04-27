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

# Old way
table4a %>% 
  gather(`1999`, `2000`, key='year', value='cases') %>% 
  arrange(country)

table4b %>% 
  gather(2:3, key='year', value = 'population') %>% 
  arrange(country)

# Actual and correct way
table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = 'year', values_to = 'cases')


table4b %>% 
  pivot_longer(2:3, names_to = 'year', values_to = 'population')

tidy4 <- table4a %>% 
  pivot_longer(2:3, names_to = 'year', values_to = 'cases') %>% 
  left_join(pivot_longer(table4b, 2:3, names_to = 'year', values_to = 'population'),
            by = c('year', 'country'))

# Old way

spread(table2, key = type, value = count)

# Actual and correct way

pivot_wider(table2, names_from = 'type', values_from = 'count')


# Exercises

people <- tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods", "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)

people %>% 
  group_by(name, key) %>% 
  mutate(obs = row_number()) %>% 
  pivot_wider(names_from = 'key', values_from = 'value') %>% 
  select(-obs)


preg <- tribble(
  ~pregnant, ~male, ~female,
  'yes', NA, 10,
  'no', 20, 12
)

preg %>% 
  pivot_longer(c('male', 'female'), names_to = 'sex', values_to = 'count')


