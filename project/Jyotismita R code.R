install.packages("tidyverse")
install.packages("plotly")
install.packages("ggplot2")
install.packages("maps")
library(tidyverse)
library(plotly)
library(ggplot2)
library(maps)
library(dplyr)

data_left <- read.csv (file = "unicef_indicator_1.csv")
data_right <-read.csv (file = "unicef_indicator_2.csv")

#exercise 1
data_join <- full_join(data_left, data_right)

data_join <- data_left %>%
  full_join(data_right, by = c("country", "current_age"))

map_world <-map_data("world")

#map
map_data_join <- left_join(map_world, data_join, by = c("region" = "country"), relationship = "many-to-many")

ggplot(data = map_data_join,  aes(x = long, y = lat, group = group, fill = obs_value.x))+
  #Add a black border to the polygons
  geom_polygon()+
  #specify the gradient colors
  scale_fill_gradient(low = "darkgreen",high = "yellow") +
  geom_polygon(color = "white") +
  labs(
    title = "Map of Observed Values of different Countries",
    x = "Longitude",
    y = "Latitude",
    fill = "Observed Value"
  ) +
  theme_classic()

# Map 2 

# Display the column names of the data_join dataset
names(data_join)

# Load the map data for world
map_world <- map_data("world")

data_filtered <- data_join %>%
  filter(as.numeric(time_period.x) == 2011) %>%
  full_join(map_world, by = c("country" = "region"), relationship = "many-to-many") %>%
  ggplot(aes(x = long, y = lat, group = group, fill = time_period.x)) +
  #Add a black border to the polygons 
  geom_polygon(color = "black")+
  geom_polygon() +
  theme_minimal()

#bar chart 1

library(ggplot2)

colnames(data_join)

data_join_filtered <- data_join %>% 
  filter("time_period"== 2011)

ggplot(data = data_join_filtered, aes(x = country, y = obs_value, fill = country)) +
  geom_bar(stat = "identity")+
  facet_wrap(~ sex, scales = "free", ncol = 1) +
  labs(
    title = "Bar Chart of Observed Values by Countries in (2012)",
    x = "Country",
    y = "Observed Value"
  ) +
  theme_minimal()

#bar chart 2

library(ggplot2)
library(dplyr)

# Assuming you have a data frame called data_join containing the required data

# Filter the data for the year 2011
data_join_filtered <- data_join %>% 
  filter(time_period.x == "2011")

# Calculate the total number of individuals in child labor for each country and gender
child_labor_counts <- data_join_filtered %>%
  group_by(country, sex.x) %>%
  summarise( indicator = sum(obs_value.x))

# Define custom color palette for each gender
color_palette <- c("Male" = "blue", "Female" = "pink", "Total" = "red")

# Create the bar plot with facets for gender
ggplot(data = child_labor_counts, aes(x = country, y = indicator, fill = sex.x)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_palette) +  # Apply custom colors
  labs(
    title = " Child Labor(thresholds) by Country and Gender in the year (2012)",
    x = "Country",
    y = "child labour thresholds"
  ) +
  facet_wrap(~ sex.x, scales = "free", ncol = 1) +  # Facet by sex
  theme_minimal()

#define selected countries
selected_countries <- c("Afghanistan", "Albania", "Bangladesh", "India", "Mexico", "Nigeria", "Ukraine", "Brazil", "Paraguay")

# Filter and process data for scatterplot
scatter_data <- data_join %>%
  filter(obs_value.x > 0) %>%
  select(country, obs_value.x, sex.x)

# Filter data for specific countries
selected_countries <- c("Afghanistan", "Albania", "Bangladesh", "India", "Mexico", "Nigeria", "Ukraine", "Brazil", "Paraguay")
scatter_data_filtered <- scatter_data %>%
  filter(country %in% selected_countries)

# Plot scatterplot with regression line
ggplot(scatter_data_filtered, aes(x = country, y = obs_value.x, color = sex.x)) +
  geom_point(size = 4) +
  geom_smooth (aes(group = sex.x) ,method = "lm", se = FALSE,color = "maroon", linetype = "dashed", size = 0.5 ) +  # Add linear regression line
  labs(
    title = "Scatter Plot of Observed Values by Countries",
    x = "Country",
    y = "Observed Value"
  ) +
  theme_minimal()

#timeseries 1

# Load required packages
library(ggplot2)

# Filter and process data for time series plot
timeseries_data <- data_join %>%
  filter(!is.na(obs_value.x))  # Remove rows with missing obs_value

# Filter data for countries with highest obs_value
top_countries <- timeseries_data %>%
  group_by(country) %>%
  summarise(max_obs_value = max(obs_value.x)) %>%
  arrange(desc(max_obs_value)) %>%
  top_n(6)  # Select top 6 countries with highest obs_value

# Print the list of top countries
cat("Top 6 countries with highest observed values:\n")
print(top_countries$country)

# Filter time series data for top countries
timeseries_data_filtered <- timeseries_data %>%
  filter(country %in% top_countries$country)

# Plot time series
ggplot(timeseries_data_filtered, aes(x = time_period.x, y = obs_value.x, color = obs_value.x)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +  # Adjust gradient colors
  labs(
    title = "Time Series Plot of Observed Values over Time (Top 6 Countries)",
    x = "Time Period",
    y = "Observed Value",
    color = "Observed Value"
  ) +
  theme_classic()

