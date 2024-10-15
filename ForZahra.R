###Load libraries
library(tidyverse)
library(ggplot2)

#Load data
master <- read.csv("MasterZahra.csv")

#Clean Data
master <- select(master, Entity.Empowered, Powers.Invoked) #Selects a few columns that we are interested in


master %>%
  filter(Entity.Empowered != "" & !is.na(Entity.Empowered)) %>%
  filter(Powers.Invoked != "" & !is.na(Powers.Invoked)) -> master #here we filter out the rows that are blank in either column


master %>%
  separate_rows(Entity.Empowered, sep = ",") %>% # Separates rows by Entity.Empowered
  separate_rows(Powers.Invoked, sep = ",") -> master_cleaned 

df <- master_cleaned %>%
  group_by(Entity.Empowered, Powers.Invoked) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(df) +
  geom_bar(aes(x=Entity.Empowered, fill=Powers.Invoked))
