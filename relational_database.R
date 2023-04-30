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




