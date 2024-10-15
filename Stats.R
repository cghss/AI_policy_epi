library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

#call data
ai.data <- read.csv("AIDataClean.csv")

ai.data <- ai.data %>%
  filter(!is.na(WHO_Region)) %>%
  filter(Country != "Democratic Peoples Republic of Korea (North Korea; DPRK)")

#first thing I want to know is, what percentage of countries have "yes" in the match column
#This is the total percentage of times I indicated that there was a direct match between what was surfaced by AI and what I found
gross.percentage.yes <- mean(ai.data$Match == "Yes") * 100
print(gross.percentage.yes)

#however, for many countries, there is ample information on the internet that shows that there is no national vaccination mandate
#It seems that AI had more trouble finding it when there was a mandate
#let's calculate the gross percentage of matches only for countries 


#first, filter the data


ai.filtered <- ai.data[!(ai.data$Status == "No relevant documents identified" & ai.data$Match == "Yes"), ]

#Calculate percentage
percentage.yes.filtered <- mean(ai.filtered$Match == "Yes") * 100
print(percentage.yes.filtered)

#Okay, now I am really interested in understanding if AI is worse at surfacing laws and regulations in some WHO regions

percentage.by.region <- ai.data %>%
  group_by(WHO_Region) %>%
  summarize(percentage_yes = mean(Match == "Yes") * 100)


#And now I am going to do the exact same thing with the percentage by region only for countries that we know have laws 
filtered.percentage.by.region <- ai.filtered %>%
  group_by(WHO_Region) %>%
  summarize(percentage_yes = mean(Match == "Yes") * 100)

data_for_plot <- ai.data %>%
  group_by(WHO_Region, Match) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

ggplot(data_for_plot, aes(x = WHO_Region, y = percentage, fill = Match)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =c("#bb5b3f", "#ffb896", "#41b989", "#156d7d")) +
  scale_x_discrete(labels = function(x) ifelse(x == "AMRO", "PAHO", x)) +
  labs(
    x = "WHO Region",
    y = "Percentage",
    fill = "Concordance"
  ) +
  theme_minimal()


data_for_plot_filtered <- ai.filtered %>%
  group_by(WHO_Region, Match) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

ggplot(data_for_plot_filtered, aes(x = WHO_Region, y = percentage, fill = Match)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =c("#bb5b3f", "#ffb896", "#41b989", "#156d7d")) +
  scale_x_discrete(labels = function(x) ifelse(x == "AMRO", "PAHO", x)) +
  labs(
    x = "WHO Region",
    y = "Percentage",
    fill = "Match Response"
  ) +
  theme_minimal()

