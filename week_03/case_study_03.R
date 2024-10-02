# Load libraries
library(ggplot2)
library(gapminder)
library(dplyr)

# Filter out Kuwait from the dataset
gapminder_filtered <- gapminder %>%
  filter(country != "Kuwait")
summary(gapminder_filtered)

# Create the first plot with ggplot, faceted by year
p1 <- ggplot(gapminder_filtered, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/100000)) +
  geom_point() +
  facet_wrap(~year, nrow = 1) +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  labs(
    x = "GDP per Capita",
    y = "Life Expectancy",
    size = "Population (100k)",
    color = "Continent"
  )

# Group by continent and year, and summarize to calculate weighted mean for the second plot
gapminder_continent <- gapminder_filtered %>%
  group_by(continent, year) %>%
  summarize(
    gdpPercapweighted = weighted.mean(gdpPercap, w = pop),
    pop = sum(pop)
  )

# Create the second plot, adding continent-level averages as black lines
p2 <- ggplot(gapminder_filtered, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_line(aes(group = country)) +
  geom_point() +
  geom_line(data = gapminder_continent, aes(x = gdpPercapweighted, y = lifeExp, group = continent), color = "black", size = 1) +
  geom_point(data = gapminder_continent, aes(x = gdpPercapweighted, y = lifeExp, size = pop), color = "black") +
  facet_wrap(~year, nrow = 1) +
  theme_bw() +
  labs(
    x = "GDP per Capita",
    y = "Life Expectancy",
    size = "Population (Total)",
    color = "Continent"
  )

# Save the plots as .png files
ggsave("plot1.png", plot = p1, width = 15, height = 6)
ggsave("plot2.png", plot = p2, width = 15, height = 6)
 
