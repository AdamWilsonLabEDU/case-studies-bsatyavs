# Load necessary libraries
library(terra)
library(sf)
library(spData)
library(tidyverse)
library(ncdf4)

# Download and load the CRU temperature data (1961-1990)
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc", "crudata.nc", method = "curl")

setwd("F:/MS GIS/3rd sem/geo 511/geo511")

# Read the data as a raster object
tmean=rast("crudata.nc")

# Inspect the raster object
print(tmean)
plot(tmean)

# Assuming 'world' contains country polygons and 'tmean' is the temperature raster
# First, calculate the maximum temperature across all raster layers
tmean_max <- max(tmean)

# Extract maximum temperature for each country in the world dataset
world_clim <- terra::extract(tmean_max, world, fun = max, na.rm = TRUE, small = TRUE)

# Bind the temperature data to the original world dataset
world_clim <- bind_cols(world, world_clim[, 2])
names(world_clim)[ncol(world_clim)] <- "max_temp"

# Plot maximum temperature for each country
ggplot(data = world_clim) +
  geom_sf(aes(fill = max_temp)) +
  scale_fill_viridis_c(name = "Maximum\nTemperature (C)") +
  theme_minimal() +
  theme(legend.position = 'bottom')

# Identify the hottest country on each continent
hottest_continents <- world_clim %>%
  group_by(continent) %>%
  top_n(1, max_temp) %>%
  select(name_long, continent, max_temp) %>%
  arrange(desc(max_temp))

# Drop geometry and display table
hottest_continents <- st_set_geometry(hottest_continents, NULL)
print(hottest_continents)

# Save results as a CSV file
write.csv(hottest_continents, "hottest_continents.csv")


 
