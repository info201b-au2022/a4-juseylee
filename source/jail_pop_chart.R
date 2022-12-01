library(ggplot2)
library(dplyr)
library(tidyr)
library("scales")

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


get_year_jail_pop <- incarceration_data[c("year","total_jail_pop")]

plot_jail_pop_for_us <- ggplot(data=get_year_jail_pop, aes(x=year, y=total_jail_pop)) +
  geom_bar(stat="identity") +
  xlab("Years") +
  ylab("Total Jail Population") +
  labs(title = "Increase of Jail Population in U.S. (1970-2018)") +
  scale_y_continuous(labels = scales :: comma)
plot_jail_pop_for_us

