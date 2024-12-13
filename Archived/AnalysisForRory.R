library(readr)
library(tidyverse)
library(dplyr)
library(countrycode)

#Drop in data
QI.rory <- read.csv("QIRawData.csv")

data_for_plot <- QI.rory %>%
  group_by(Match) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

print(data_for_plot)
