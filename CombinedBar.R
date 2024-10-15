#Q+I Bar
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)

#load data
QI.data <- read.csv("QIDataClean.csv")

QI.data <- QI.data %>%
  filter(!is.na(WHO_Region)) 

# List of English-speaking countries
english_countries <- c(
  "Antigua and Barbuda", "Australia", "Bahamas", "Barbados", "Belize",
  "Bhutan", "Botswana", "Brunei Darussalam", "Cameroon", "Canada",
  "Chinese Taipei (Taiwan; Republic of China; ROC)", "Cook Islands", "Dominica",
  "Eswatini (Swaziland)", "Fiji", "Gambia (Republic of The)", "Ghana", "Grenada",
  "Guyana", "India", "Ireland", "Jamaica", "Kenya", "Kiribati", "Lesotho",
  "Liberia", "Malawi", "Malaysia", "Malta", "Marshall Islands", "Mauritius",
  "Micronesia (Federated States of Micronesia)", "Namibia", "Nauru", "Nepal",
  "New Zealand", "Nigeria", "Niue", "Pakistan", "Palau", "Papua New Guinea",
  "Philippines", "Saint Kitts and Nevis (St Kitts and Nevis)", "Saint Lucia (St Lucia)",
  "Saint Vincent and the Grenadines (St Vincent and the Grenadines)", "Samoa",
  "Seychelles", "Sierra Leone", "Singapore", "Solomon Islands", "South Africa",
  "Sri Lanka", "Tonga", "Trinidad and Tobago", "Tuvalu", "Uganda",
  "United Kingdom of Great Britain and Northern Ireland (UK)", "United Republic of Tanzania",
  "United States of America (USA)", "Vanuatu", "Zambia", "Zimbabwe"
)

# Add the 'English' column to QIData
QI.data <- QI.data %>%
  mutate(English = ifelse(Country %in% english_countries, "YES", "NO"))

gross.percentage.yes <- mean(QI.data$Concordance == "Yes") * 100
print(gross.percentage.yes)

qi.filtered.percentage.by.region <- QI.data %>%
  group_by(WHO_Region) %>%
  summarize(percentage_yes = mean(Match == "Yes") * 100)


english_countries_yes <- QI.data %>%
  filter(English == "YES")

english_countries_no <- QI.data %>%
  filter(English == "NO")

# Calculate the percentage of English-speaking countries with Concordance
percentage_english_concordance_yes <- mean(english_countries_yes$Concordance %in% c("Yes", "No - Provides More Info")) * 100
print(percentage_english_concordance_yes)

# Calculate the percentage of non-English-speaking countries with Concordance 
percentage_english_concordance_no <- mean(english_countries_no$Concordance %in% c("Yes", "No - Provides More Info")) * 100
print(percentage_english_concordance_no)

percentage.by.region <- QI.data %>%
  group_by(WHO_Region) %>%
  summarize(percentage_yes = mean(Concordance == "Yes") * 100)

data_for_rory <- QI.data %>%
  group_by(Concordance) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


data_for_plot <- QI.data %>%
  group_by(WHO_Region, Concordance) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()



QIBar <- ggplot(data_for_plot, aes(x = WHO_Region, y = percentage, fill = Concordance)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =c("#bb5b3f", "#ffb896", "#41b989", "#156d7d")) +
  scale_x_discrete(labels = function(x) ifelse(x == "AMRO", "PAHO", x)) +
  labs(
    x = "WHO Region",
    y = "Percentage",
    fill = "Concordance Response"
  ) +
  theme_minimal()

print(QIBar)

#Vaxx Bar - topline
ai.data <- read.csv("AIDataClean.csv")

ai.data <- ai.data %>%
  filter(!is.na(WHO_Region))

gross.percentage.yes <- mean(ai.data$Match == "Yes") * 100

percentage.by.region <- ai.data %>%
  group_by(WHO_Region) %>%
  summarize(percentage_yes = mean(Match == "Yes") * 100)

data_4_plot <- ai.data %>%
  group_by(WHO_Region, Match) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

VaxxBar <- ggplot(data_4_plot, aes(x = WHO_Region, y = percentage, fill = Match)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =c("#41b989", "#ffb896", "#bb5b3f", "#156d7d")) +
  scale_x_discrete(labels = function(x) ifelse(x == "AMRO", "PAHO", x)) +
  labs(
     x = "WHO Region",
     y = "Percentage",
     fill = "Concordance Response"
  ) +
  theme_minimal()

combined_plot <- (QIBar + VaxxBar) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")  # Optional: move the combined legend to the bottom

combined_plot <- combined_plot + 
  labs(x = "WHO Region", y = "Percentage")

# Display the combined plot with a shared legend
print(combined_plot)

