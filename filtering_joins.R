{
  library(tidyverse)
  library(nycflights13)
}

top <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)

# Normal filter

flights %>%
  filter(dest %in% top$dest)

# Filter join

flights %>%
  semi_join(top)



# Find the 10 days with the highest average delay and filter
# Normal filter

flights %>%
  group_by(month, day) %>%
  mutate(
    avg_delay = mean(arr_delay, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    rank = min_rank(desc(avg_delay))
  ) %>%
  filter(rank <= 10)

# Filter join
(top <- flights %>%
    group_by(month, day) %>%
    summarise(
      avg_delay = mean(arr_delay, na.rm = TRUE),
    ) %>%
    ungroup() %>%
    filter(min_rank(desc(avg_delay)) <= 10))

flights %>%
  semi_join(top)


# Exercises

flights %>% 
  anti_join(planes, by = 'tailnum') %>% 
  select(year:day, carrier, flight, tailnum, origin, dest) %>% 
  count(carrier, sort = TRUE)

# Filter flights to only show flights with planes that have flown at least 100
# flights

flights %>%
  semi_join(
    flights %>% 
      filter(!is.na(tailnum)) %>% 
      count(tailnum) %>% 
      filter(n >= 100)
  )

# Find the 48 hours that have the worst delays. Cross reference it with the
# weather data

worst_hours <- flights %>% 
  select(origin, month, day, hour, dep_delay, sched_dep_time) %>% 
  group_by(origin, month, day, hour) %>% 
  summarise(dep_delay = mean(dep_delay)) %>% 
  ungroup() %>% 
  arrange(desc(dep_delay)) %>% 
  head(48)

weather %>% 
  semi_join(worst_hours) %>% 
  select(!c(origin:hour))


