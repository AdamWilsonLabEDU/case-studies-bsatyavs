install.packages("tidyverse")
library(tidyverse)

# Define the URL to the temperature data
dataurl <- "https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1"

# Load the data, skipping the first row and renaming columns for clarity
temp <- read_csv(dataurl, 
                 na = "999.90",  # Treat 999.90 as missing data
                 skip = 1,  # Skip the first row
                 col_names = c("YEAR", "JAN", "FEB", "MAR", 
                               "APR", "MAY", "JUN", "JUL", 
                               "AUG", "SEP", "OCT", "NOV", 
                               "DEC", "DJF", "MAM", "JJA", 
                               "SON", "metANN"))
# Summary of the dataset
summary(temp)

# Glimpse to view the structure
glimpse(temp)

# View the dataset in a spreadsheet-like format
View(temp)

# Create the time series plot
ggplot(temp, aes(x = YEAR, y = JJA)) +
  geom_line(color = "blue", size = 1) +  # Line plot for the raw data
  geom_smooth(method = "loess", color = "red") +  # Smoothed trend line
  xlab("Year") +
  ylab("Mean Summer Temperature (°C)") +
  ggtitle("Mean Summer Temperatures (June-August) at Buffalo",
          subtitle = "Data source: NASA GISS Surface Temperature Analysis") 

# Save the plot as a PNG file
ggsave("Buffalo_summer_temps.png", width = 8, height = 6)
