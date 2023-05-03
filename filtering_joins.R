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
flights