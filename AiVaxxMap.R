library(tidyr)
library(readr)
library(dplyr)
library(rnaturalearth)
library(ggplot2)

#call data
ai.data <- read.csv("AIDataClean.csv")

ai.data <- ai.data %>%
  filter(!is.na(WHO_Region)) %>%
  filter(Country != "Democratic Peoples Republic of Korea (North Korea; DPRK)")

map <- ne_countries(type = 'countries')

one.df <- ai.data%>%
  filter(grepl("1. Is there a law that requires routine childhood vaccination in ? ", Query, ignore.case = TRUE)) %>%
  data.frame()

two.df <- ai.data %>%
  filter(grepl("2. In the event of an emergency, can the government require a citizen to receive a vaccination in?", Query, ignore.case = TRUE)) %>%
  data.frame()

one.data <- full_join(one.df, map, by = c("ISO" = "iso_a3_eh"))
two.data <- full_join(two.df, map, by = c("ISO" = "iso_a3_eh"))

one.map <- ggplot() +
  geom_sf(data = one.data, aes(fill = Match, geometry = geometry), color = "black", size = 0.2) +
  scale_fill_manual(values = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white"), 
                    na.value = "white", 
                    guide = guide_legend(override.aes = list(fill = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white")))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_blank()) + 
  ggtitle("A")
print(one.map) #Childhood vaccination

two.map <- ggplot() +
  geom_sf(data = two.data, aes(fill = Match, geometry = geometry),color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(values = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white"), 
                    na.value = "white", 
                    guide = guide_legend(override.aes = list(fill = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white")))) +
  theme(
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()) + 
  ggtitle("B")
print(two.map) #Emergency vaxx

library(patchwork)

one.map + two.map +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 


QI.data <- read.csv("QIDataClean.csv")

QI.data <- QI.data %>%
  filter(!is.na(WHO_Region)) 

map <- ne_countries(type = 'countries')

iso.df <- QI.data %>%
  filter(grepl("1. What law allows the government to isolate?", Query, ignore.case = TRUE)) %>%
  data.frame()

qur.df <- QI.data %>%
  filter(grepl("6. What level of government quarantines?", Query, ignore.case = TRUE)) %>%
  data.frame()

iso.data <- full_join(iso.df, map, by = c("ISO" = "iso_a3_eh"))
qur.data <- full_join(qur.df, map, by = c("ISO" = "iso_a3_eh"))

iso.map <- ggplot() +
  geom_sf(data = iso.data, aes(fill = Concordance, geometry = geometry),color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(values = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white"), 
                    na.value = "white", 
                    guide = guide_legend(override.aes = list(fill = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white")))) + 
  theme(
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()) + 
  ggtitle("A")
print(iso.map) #Iso Q1

qur.map <- ggplot() +
  geom_sf(data = qur.data, aes(fill = Concordance, geometry = geometry),color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(values = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white"), 
                    na.value = "white", 
                    guide = guide_legend(override.aes = list(fill = c("#bb5b3f", "#ffb896", "#41b989", "#156d7d", "white")))) +
  theme(
    plot.title = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()) +
  ggtitle("B")
print(qur.map) #Quar Q6

iso.map + qur.map +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 
