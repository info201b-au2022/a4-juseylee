library(mapproj)
library(dplyr)
library(maps)
library(ggplot2)
library(scales)

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE) 

state_abv <- data.frame(state.abb, state.name)
state_abv <- state_abv %>%
  add_row(state.abb = "WA", state.name = "Washington")

state_shape <- map_data('state')

male_jail_pop_state <- incarceration_data %>% 
  group_by(state) %>%
  filter(year == "2008") %>%
  summarize(total_male = sum(male_jail_pop, na.rm= TRUE))

male_jail_pop_state <-left_join(male_jail_pop_state, state_abv, by = c('state'='state.abb'))

male_jail_pop_state <- male_jail_pop_state %>% mutate(region = tolower(state.name))

state_shape_male <- left_join(state_shape, male_jail_pop_state, by = 'region')

ggplot(state_shape_male) +
  geom_polygon(aes(x=long, y= lat, fill = total_male, group = group)) +
  coord_map() +
  labs(title = 'Total Number of Male in Jail in 2008', x= 'Longitude', y= 'Latitude', fill= 'Number of Male Prisoners') +
  scale_fill_continuous(low = '#132B43', high = 'red', labels = scales::label_number())

