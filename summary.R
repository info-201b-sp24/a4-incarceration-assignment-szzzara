library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(usmap)

# loading the vera datasets, i will focus on the first two
prison_pop <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")

# i want the focus to be comparisons of gender and race, 
# so i will remove other columns
prison_pop <- prison_pop %>% 
  filter(!is.na(black_prison_pop) & !is.na(white_prison_pop))
  select(total_prison_pop, female_prison_pop, male_prison_pop, black_prison_pop,
         white_prison_pop, year, urbanicity, county_name, state, fips)

# introduction and summary (will do the descriptions in the markdown file)
# Trends Over Time: comparing the patterns before and after 2000
# plotting using a line plot so i can show two variables over one x axis (time)
ggplot(prison_pop, aes(x = year)) +
  geom_line(aes(y = black_prison_pop, color = "Black Prison Population")) +
  geom_line(aes(y = white_prison_pop, color = "White Prison Population")) +
  labs(title = "Black and White Prison Populations Over Time",
       x = "Year",
       y = "Prison Population") +
  scale_color_manual(values = c("Black Prison Population" = "pink", "White Prison Population" = "blue")) +
  theme_minimal()

# now the Variable Comparison Chart, first cleaning
# i want this chart to be comparing different living areas and their
# differing prison populations
second_prison_pop <- prison_pop %>%
  filter(!is.na(urbanicity), !is.na(total_prison_pop)) %>%
  group_by(urbanicity) %>%
  summarize(total_prison_pop = sum(total_prison_pop, na.rm = TRUE))

# making bar graph
second_chart <- ggplot(second_prison_pop, aes(x = urbanicity, y = total_prison_pop, fill = urbanicity)) +
  geom_bar(stat = "identity") +
  labs(title = "Prison Population by Geographic Type",
       x = "Geographic Type", y = "Total Prison Population",
       fill = "Area Type") +
  scale_y_continuous(labels = comma_format()) +
  theme_minimal()

# printing graph
print(second_chart)

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