library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(usmap)

# now the Geographical Distribution Chart
# summarizing the total prison population by state, not county
library(scales)
state_prison_pop <- prison_pop %>%
  filter(!is.na(total_prison_pop)) %>%
  group_by(state) %>%
  summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE))

# getting the US states map data
us_states <- map_data("state")
state_lookup <- data.frame(
  abbreviation = state.abb,
  full_name = tolower(state.name)
)

# creating a lookup table for state abbreviations and full state names
state_lookup <- data.frame(
  abbreviation = state.abb,
  full_name = tolower(state.name)
)

# changing the column to be abbreviated states instead of full state name
prison_pop <- prison_pop %>%
  left_join(state_lookup, by = c("state" = "abbreviation"))

state_prison_pop <- prison_pop %>%
  filter(!is.na(total_prison_pop)) %>%
  group_by(full_name) %>%
  summarise(total_prison_pop = sum(total_prison_pop, na.rm = TRUE))

# merging the summarized data with the map data
map_data <- us_states %>%
  left_join(state_prison_pop, by = c("region" = "full_name"))

# plotting map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_prison_pop)) +
  geom_polygon(color = "white") +
  scale_fill_continuous(low = "yellow", high = "red", na.value = "grey50", name = "Total Prison Population", labels = comma) +
  theme_minimal() +
  labs(title = "Total Prison Population by State", x = "", y = "") +
  scale_y_continuous(labels = comma_format()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.grid = element_blank())
