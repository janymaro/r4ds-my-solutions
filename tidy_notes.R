{
  library(tidyverse)
  library(hexbin)
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

tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods", "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
) %>% 
print() %>% 
  group_by(name, key) %>% 
  mutate(obs = row_number()) %>% 
  pivot_wider(names_from = 'key', values_from = 'value') %>% 
  select(-obs)


tribble(
  ~pregnant, ~male, ~female,
  'yes', NA, 10,
  'no', 20, 12
) %>%
  print() %>% 
  pivot_longer(c('male', 'female'), names_to = 'sex', values_to = 'count')



# <--    Push and Pull Unite Separate    -->


table3 %>% 
  separate(
    rate,
    into = c('cases', 'population'),
    convert = TRUE
  )

table5 %>% 
  unite(new, century, year, sep = '')

tibble(x = c("a,b,c", "d,e,f,g", 'h,i,j')) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c('a,b,c', 'd,e', 'f,g,i')) %>% 
  separate(x, c('one', 'two', 'three'), fill = "left")

# <--     Real Problem(who)     -->>

(who1 <- who %>% 
  pivot_longer(
    new_sp_m014:newrel_f65,
    names_to = 'key',
    values_to = 'cases',
    values_drop_na = TRUE
  ) %>%
  mutate(
    key = stringr::str_replace(key, 'newrel', 'new_rel')
  ) %>% 
  separate(
    key,
    into = c('new', 'code', 'sex_age'),
    sep = '_'
  ) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(
    sex_age,
    into = c('sex', 'age'),
    sep = 1
  ))

# For each country, year, sex compute the total number of cases. Make an
# informative visualization of the data

who_whitout_code <- who1 %>% 
  group_by(country, year, sex) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup() 

who_whitout_code %>% 
  ggplot(aes(year, cases, color = sex)) +
  geom_smooth(se = FALSE) +
  coord_cartesian(xlim = c(1997, 2016)) +
  facet_wrap(~country)

who_whitout_code %>% 
  ggplot(aes(year, cases, color = sex)) +
  geom_line() +
  facet_wrap(~country)

who_whitout_code %>% 
  ggplot(aes(country, cases, fill = sex)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  facet_wrap(~cut_width(year, 1, boundary = 0))
  
who_whitout_code %>% 
  ggplot(aes(cut_number(year, 6), country)) +
  geom_tile(aes(fill = cases)) +
  facet_wrap(~sex)

# who1 %>% 
#   group_by(country, year, sex) %>% 
#   summarise(cases = sum(cases)) %>% 
#   ungroup() %>% 
#   ggplot(
#     aes(
#       cut_width(year, 1, boundary = 0), 
#       cases,
#       color = sex
#     )
#   ) +
#   geom_boxplot() +
#   coord_flip() +
#   facet_wrap(~country)


  
  