library(readr)
library(tidyverse)
library(dplyr)
library(countrycode)

#Drop in data
QI.raw <- read.csv("QIRawData.csv")

#WHO Regions
who.raw <- read.csv("WHO_Regions_AI.csv")

#Mutation to make the categories useful
QI.raw <- QI.raw %>%
  mutate(Concordance = case_when(
    Match == "Yes" ~ "Yes",
    Match == "No - AI provides additional info missed by human" ~ "No - Provides More Info",
    Match == "No - AI provides correct info, human was entirely wrong" ~ "No - Provides More Info",
    Match == "No - human relying on out of date COVID law" ~ "No - Provides More Info",
    Match == "AI provides correct info, human was entirely wrong" ~ "No- Misses Information",
    Match == "AI info was not relevant" ~ "No- Misses Information",
    Match == "Exact match in answer, but no law found" ~ "No- Misses Information",
    Match == "No - AI misses info found by human" ~ "No- Misses Information",
    Match == "No - Both AI misses info found by human and human misses info found by AI" ~ "No- Misses Information",
    Match == "Answer match, wrong law" ~ "No- Misses Information",
    Match == "No - AI provides wrong info" ~ "No - Wrong Info",
    TRUE ~ "Missing Info"  
  ))


#add ISOs
QI.raw$ISO <- countrycode(sourcevar = QI.raw$Country, origin = "country.name", destination = "iso3c")

#add lost countries
lost.countries <- data.frame(Country.Name = c("Cote d'Ivoire", "Liechtenstein", "Niue"), 
                             ISO3 = c("CIV", "LIE", "NIU"), 
                             WHO_Region = c("AFRO", "EURO", "WPRO"))

#add lost countries to the WHO dataset
who.raw <- rbind(who.raw, lost.countries)

#Put together the military dataset and the who regions
QI.data <- left_join(QI.raw, who.raw, by = c("ISO" = "ISO3"))

#write csv
write.csv(QI.data, "/Users/ciaraweets/Dropbox/R Projects/AI_policy_epi/QIDataClean.csv", row.names = FALSE)

