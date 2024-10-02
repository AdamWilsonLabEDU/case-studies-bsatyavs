install.packages("tidyverse")
install.packages("nycflights13")
library(tidyverse)
library(nycflights13)
view(airlines)
view(planes)
view(weather)
view(airports)
view(flights)


# Find the farthest airport
airports %>%
  semi_join(flights, by = c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# Join flights and airports to find the maximum distance
flights_joined <- left_join(flights, airports, by = c("dest" = "faa"))

# Filter to find the farthest airport
farthest_airport <- flights_joined %>%
  filter(distance == max(distance)) %>%
  select(name) %>%
  distinct()

# Display the farthest airport
print(farthest_airport)
 
