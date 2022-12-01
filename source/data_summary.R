incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library("dplyr")
library("tidyverse")

incarceration_data <- incarceration_data[!is.na(incarceration_data$female_jail_pop),]
incarceration_data <- incarceration_data[!is.na(incarceration_data$male_jail_pop),]

female_pop_filter_state <- incarceration_data %>%
  group_by(state) %>% 
  summarise(across(female_jail_pop, sum))

male_pop_filter_state <- incarceration_data %>%
  group_by(state) %>% 
  summarise(across(male_jail_pop, sum))

female_pop_filter_year <- incarceration_data %>%
  group_by(year) %>% 
  summarise(across(female_jail_pop, sum))

male_pop_filter_year <- incarceration_data %>%
  group_by(year) %>% 
  summarise(across(male_jail_pop, sum))
  

summary_info <- list()

# Average jail population count
summary_info$female_jail_ct <- female_pop_filter_state %>%
  summarize(female_jail_ct = round(mean(female_jail_pop, na.rm = T)), 2) %>%
  pull(female_jail_ct)

summary_info$male_jail_ct <- male_pop_filter_state %>%
  summarize(male_jail_ct = round(mean(male_jail_pop, na.rm = T)), 2) %>%
  pull(male_jail_ct)

# States with the highest number of jail population
summary_info$max_female_jail_st <- female_pop_filter_state %>%
  group_by(state) %>%
  filter(female_jail_pop == max(female_jail_pop, na.rm = T)) %>%
  pull(state)

summary_info$max_male_jail_st <- male_pop_filter_state %>%
  group_by(state) %>%
  filter(male_jail_pop == max(male_jail_pop, na.rm = T)) %>%
  pull(state)

# States with the lowest number of jail population
summary_info$min_female_jail_st <- female_pop_filter_state %>%
  group_by(state) %>%
  filter(female_jail_pop == min(female_jail_pop, na.rm = T)) %>%
  pull(state)

summary_info$min_male_jail_st <- male_pop_filter_state %>%
  group_by(state) %>%
  filter(male_jail_pop == min(male_jail_pop, na.rm = T)) %>%
  pull(state)

# Years with the highest number of jail population 
summary_info$max_female_jail_yr <- female_pop_filter_year %>%
  group_by(year) %>%
  filter(female_jail_pop == max(female_jail_pop, na.rm = T)) %>%
  pull(year)

summary_info$max_male_jail_yr <- male_pop_filter_year %>%
  group_by(year) %>%
  filter(male_jail_pop == max(male_jail_pop, na.rm = T)) %>%
  pull(year)

# Years with the lowest number of jail population 
summary_info$min_female_jail_yr <- female_pop_filter_year %>%
  group_by(year) %>%
  filter(female_jail_pop == min(female_jail_pop, na.rm = T)) %>%
  pull(year)

summary_info$min_male_jail_yr <- male_pop_filter_year %>%
  group_by(year) %>%
  filter(male_jail_pop == min(male_jail_pop, na.rm = T)) %>%
  pull(year)



View(summary_info)