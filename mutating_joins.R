{
  library(tidyverse)
  library(nycflights13)
}



left_join(
  tribble(       # x
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    2, "x3",
    1, "x4"
  ),
  tribble(      # y
    ~key, ~val_y,
    1, "y1",
    2, "y2"
  ),
  by = 'key'
)

flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier) %>%
  left_join(weather)

# Exercises
# Compute the average delay by destination, then join on the air
# ports data frame so you can show the spatial distribution of delays. Here’s an
# easy way to draw a map of the United States:

airports %>%
  semi_join(flights, c("faa" = 'dest')) %>%
  ggplot(aes(lon, lat)) +
  borders('state') +
  geom_point() +
  coord_quickmap()

airports %>%
  semi_join(flights, c('faa' = 'dest'))


# arrow version

flights %>%
  select(
    flight,
    tailnum,
    dep_delay,
    arr_delay,
    carrier,
    origin,
    dest
  ) %>%
  group_by(origin, dest) %>%
  summarise(
    arr_delay_avg = mean(arr_delay, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  inner_join(
    airports %>% select(faa, origin_lat = lat, origin_lon = lon),
    by = c('origin' = 'faa')
  ) %>%
  inner_join(
    airports %>% select(faa, dest_lat = lat, dest_lon = lon),
    by = c('dest' = 'faa')
  ) %>%
  ggplot() +
  borders('state') +
  geom_segment(
    aes(
      x = origin_lon,
      y = origin_lat,
      xend = dest_lon,
      yend = dest_lat,
      color = arr_delay_avg
    ),
    arrow = arrow(length = unit(0.1, 'cm'))
  ) +
  coord_quickmap(xlim = c(-130, -70), ylim = c(25, 50))


# Point version by size

flights %>%
  select(
    flight,
    tailnum,
    dep_delay,
    arr_delay,
    carrier,
    origin,
    dest
  ) %>%
  group_by(dest) %>%
  summarise(
    arr_delay_avg = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(airports, by = c('dest' = 'faa')) %>%
  ggplot(aes(lon, lat)) +
  borders('state') +
  geom_point(aes(size = arr_delay_avg, alpha = 1/10)) +
  coord_quickmap()

# By color

flights %>%
  select(
    flight,
    tailnum,
    dep_delay,
    arr_delay,
    carrier,
    origin,
    dest
  ) %>%
  group_by(dest) %>%
  summarise(
    arr_delay_avg = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  inner_join(airports, by = c('dest' = 'faa')) %>%
  ggplot(aes(lon, lat)) +
  borders('state') +
  geom_point(aes(color = arr_delay_avg)) +
  coord_quickmap()


# Exercise 2: Add the location of the origin and destination (i.e, the lat and
# lon) to flights

flights %>%
  left_join(
    airports %>% select(origin = faa, origin_lat = lat, origin_lon = lon),
    by = 'origin'
  ) %>%
  left_join(
    airports %>% select(dest = faa, dest_lat = lat, dest_lon = lon),
    by = 'dest'
  )

# Exercise 3: Is there a relationship between the age of a plane and its delays

flights_with_age <- flights %>%
  select(
    tailnum,
    arr_delay,
    dep_delay,
    year
  ) %>%
  inner_join(
    select(planes, manufactured_year = year, tailnum),
    by = 'tailnum'
  ) %>%
  mutate(
    age = year - manufactured_year
  ) %>%
  select(-manufactured_year, -year) %>%
  filter(!is.na(age), !is.na(arr_delay), !is.na(dep_delay))

# Total smooth plot arr_delay
flights_with_age %>%
  ggplot(aes(age, arr_delay)) +
  geom_smooth()

# Departure Delay
flights_with_age %>%
  ggplot(aes(age, dep_delay)) +
  geom_smooth()

# Limited Age Histogram
flights_with_age %>%
  ggplot(aes(age)) +
  geom_histogram() +
  coord_cartesian(ylim = c(0, 5000))

flights_with_age %>%
  group_by(age = cut_width(age, 5, boundary = 0)) %>%
  summarise(
    n = n(),
  )

# Grouped by age and smooth graph

flights_with_age %>%
  mutate(age = if_else(age > 30, 30L, age)) %>%
  group_by(age) %>%
  summarise(
    n = n(),
    arr_delay = mean(arr_delay),
    dep_delay = mean(dep_delay)
  ) %>%
  ggplot(aes(age, arr_delay)) +
  geom_smooth()


# Exercise 4: What weather conditions make it more likely to see a delay

(flights_weather <- flights %>%
    select(
      year:day,
      hour,
      contains('delay'),
      origin,
    ) %>%
    filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
    inner_join(
      select(weather, -time_hour),
      by = c(
        'origin',
        'year',
        'month',
        'day',
        'hour'
      )
    ))


# Temperature
flights_weather %>%
  filter(!is.na(temp)) %>%
  group_by(temp) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(temp, delay)) +
  geom_point() +
  geom_smooth() +
  geom_line()

# Wind Direction
flights_weather %>%
  filter(!is.na(wind_dir)) %>%
  group_by(wind_dir) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(wind_dir, delay)) +
  geom_point() +
  geom_smooth()

# Wind Speed
flights_weather %>%
  filter(!is.na(wind_speed)) %>%
  group_by(wind_speed) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(wind_speed, delay)) +
  geom_point() +
  geom_smooth()

# Wind Gust
flights_weather %>%
  filter(!is.na(wind_gust)) %>%
  group_by(wind_gust) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(wind_gust, delay)) +
  geom_point() +
  geom_smooth()

# Precipitations
flights_weather %>%
  filter(!is.na(precip)) %>%
  group_by(precip) %>%
  summarise(delay = mean(arr_delay)) %>%
  ggplot(aes(precip, delay)) +
  geom_point() +
  geom_line() +
  geom_smooth()

# Pressure
flights_weather %>%
  filter(!is.na(pressure)) %>%
  group_by(pressure) %>%
  summarise(delay = mean(arr_delay)) %>%
  ggplot(aes(pressure, delay)) +
  geom_point() +
  geom_smooth()

# Visibility
flights_weather %>%
  filter(!is.na(visib)) %>%
  group_by(visib) %>%
  summarise(delay = mean(arr_delay)) %>%
  ggplot(aes(visib, delay)) +
  geom_point() +
  geom_line() +
  geom_smooth()


# Exercise 5: Display the spatial pattern of delays for June 13, 2013

flights %>%
  filter(month == 6, day == 13) %>%
  select(year:day, hour, arr_delay, origin, dest) %>%
  group_by(dest) %>% 
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  inner_join(
    select(airports, lat, lon, faa),
    by = c('dest' = 'faa')) %>% 
  ggplot(aes(lon, lat, size = arr_delay, colour = arr_delay)) +
  geom_point() +
  borders('state') +
  coord_quickmap() + 
  scale_colour_viridis_c()



