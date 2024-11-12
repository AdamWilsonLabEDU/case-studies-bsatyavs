# Load required libraries
library(spData)
library(sf)
library(tidyverse)
library(units)  # Optional, but useful for unit conversions

# Load the datasets
data(world)
data(us_states)

# Define the Albers equal area projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Transform the world dataset to Albers projection
world_albers <- st_transform(world, crs = albers)

# Filter the world dataset to include only Canada
canada <- world_albers %>% filter(name_long == "Canada")

# Buffer Canada by 10km (10000m)
canada_buffer <- st_buffer(canada, dist = 10000)

# Transform the US states dataset to Albers projection
us_states_albers <- st_transform(us_states, crs = albers)

# Filter the US states dataset to include only New York
ny_state <- us_states_albers %>% filter(NAME == "New York")

# Create a 'border' object by intersecting the buffered Canada with New York
border <- st_intersection(canada_buffer, ny_state)

# Plot the border area
ggplot() +
  geom_sf(data = ny_state, fill = "lightgray") +  # Fill New York state
  geom_sf(data = border, fill = "red", color = "black") +  # Highlight the border area
  labs(title = "New York Land within 10km of Canadian Border") +
  theme_minimal()

# Calculate the area of the polygon in square meters
area_sq_m <- st_area(border)

# Convert the area to square kilometers
area_sq_km <- set_units(area_sq_m, km^2)

# Print the area
print(area_sq_km)
