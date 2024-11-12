
---
title: "Case Study 8"
author: "Satya"
date: "2024-10-30"
format:
  html: default
  gfm: default
  pptx: default
  docx: default
---

CO2 Growth Rates at Mona Loa Observatory

```{r message=FALSE, echo=F}
# Loading required libraries
library(readr)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

# Defining the data URL
url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_gr_mlo.txt"

# Reading the data, skipping initial lines and naming columns
co2_data <- read_table(url, skip = 58, col_names = c("year", "mean", "uncertainty"))

```

Annual Mean Growth Rates of CO2 levels over time

```{r message=FALSE, echo=F}
# Plotting the annual mean growth rates of CO2 levels over time
ggplot(co2_data, aes(x = year, y = mean)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = mean - uncertainty, ymax = mean + uncertainty), alpha = 0.2, fill = "lightblue") +
  labs(
    title = "Annual mean growth rates of CO2 (1959 - Present)",
    x = "Year",
    y = "CO2 Concentration (ppm)"
  ) +
  theme_minimal()

```

Top Five Annual Mean Growth Rates of CO2

```{r message=FALSE, echo=F}
### Step 5: Generate a Summary Table

co2_data %>%
  tail(6) %>%
  kable(escape = FALSE, align = "c") %>%
  kable_styling(full_width = FALSE)

```

