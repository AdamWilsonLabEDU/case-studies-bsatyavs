library(sf)
library(tidyverse)
library(spData)
library(lubridate)
library(dplyr)

# Define the data URL
dataurl <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.NA.list.v04r01.csv"

# Read the storm data
storm_data <- read_csv(dataurl)

# Extract year, filter for 1950-present, replace -999.0 with NA, and add decade column
storm_data <- storm_data %>%
  mutate(year = year(ISO_TIME)) %>%
  filter(year >= 1950) %>%
  mutate_if(is.numeric, ~ ifelse(. == -999.0, NA, .)) %>%
  mutate(decade = floor(year / 10) * 10)

# Convert to spatial object
storms <- st_as_sf(storm_data, coords = c("LON", "LAT"), crs = 4326)

# Get bounding box
region <- st_bbox(storms)

# Plot the world with storm density by decade
ggplot(data = world) +
  geom_sf() +
  facet_wrap(~decade) +
  stat_bin2d(data = storms, aes(x = st_coordinates(storms)[,1], y = st_coordinates(storms)[,2]), bins = 100) +
  scale_fill_distiller(palette = "YlOrRd", trans = "log", direction = -1, breaks = c(1, 10, 100, 1000)) +
  coord_sf(ylim = region[c(2, 4)], xlim = region[c(1, 3)]) +
  labs(title = "Storm Density by Decade", fill = "Storm Count (log scale)")

# Ensure us_states is reprojected to the storm data CRS
states <- us_states %>%
  st_transform(st_crs(storms)) %>%
  select(state = NAME)

# Perform spatial join
storm_states <- st_join(storms, states, join = st_intersects, left = FALSE)

# Count unique storms per state
state_storm_counts <- storm_states %>%
  group_by(state) %>%
  summarize(storms = n_distinct(NAME)) %>%
  arrange(desc(storms)) %>%
  slice(1:5)

# Display top 5 states by storm count
state_storm_counts
 
 
