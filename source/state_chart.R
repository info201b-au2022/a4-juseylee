library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library("scales")

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

get_jail_pop_by_states <- incarceration_data[c("year","total_jail_pop", "state")] 
get_jail_pop_by_states <- filter(get_jail_pop_by_states, state %in%  c("WA", "OR", "NY", "CA"))
get_jail_pop_by_states[is.na(get_jail_pop_by_states)] <- 0
get_jail_pop_by_states <- get_jail_pop_by_states %>%
  group_by(year, state) %>%
  summarise(across(c(total_jail_pop), sum)) 

plot_jail_pop_by_states <- ggplot(data = get_jail_pop_by_states, aes(x = year, y = total_jail_pop, color = state, group= state)) + 
  geom_line(aes(color=state)) +
  labs(x= "Year", y= "Total Jail Population", title= "The Population of Prisoners in 4 Different States") +
  geom_point(aes(color=state))
plot_jail_pop_by_states
