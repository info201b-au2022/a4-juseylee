library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library("scales")
library(gridExtra)


incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

get_jail_pop <- incarceration_data[c("year","female_jail_pop", "male_jail_pop", "state")] 
get_jail_pop <- filter(get_jail_pop, state %in%  c("WA", "OR", "NY", "CA"))
get_jail_pop <- get_jail_pop %>%
  group_by(year, state) %>%
  summarise(across(ends_with("pop"), ~sum(., na.rm = TRUE)))


plot_jail_pop_female <- ggplot(data = get_jail_pop, aes(x = year, y = female_jail_pop, color = state, group= state)) + 
  geom_line(aes(color=state)) +
  xlab("Year") +
  ylab("Female Jail Population") +
  geom_point(aes(color=state))
plot_jail_pop_female

plot_jail_pop_male <- ggplot(data = get_jail_pop, aes(x = year, y = male_jail_pop, color = state, group= state)) + 
  geom_line(aes(color=state)) +
  xlab("Year") +
  ylab("Male Jail Population") +
  geom_point(aes(color=state))
plot_jail_pop_male


compare_gender_chart <- grid.arrange(plot_jail_pop_female, plot_jail_pop_male, nrow=2)

