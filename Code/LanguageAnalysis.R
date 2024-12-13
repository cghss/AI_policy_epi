library(tidyverse)
library(ggplot2)
library(dplyr)

QI.data <- read.csv("QIDataClean.csv")

QI.data <- QI.data %>%
  filter(!is.na(WHO_Region)) 



# List of countries using Chinese as an official language (ISO-3 codes)
chinese_countries_iso <- c("CHN", "SGP")

# List of countries using Arabic as an official language (ISO-3 codes)
arabic_countries_iso <- c("DZA", "BHR", "COM", "DJI", "EGY", "IRQ", 
                          "JOR", "KWT", "LBN", "LBY", "MRT", "MAR", 
                          "OMN", "QAT", "SAU", "SOM", "SDN", 
                          "SYR", "TUN", "ARE", "YEM", "TCD", "ERI")

# List of countries using Russian as an official or national language (ISO-3 codes)
russian_countries_iso <- c("RUS", "BLR", "KAZ", "KGZ", "UKR", 
                           "EST", "LVA", "MDA", "GEO", "AZE", 
                           "ARM", "UZB", "TJK", "TKM")

# List of countries using French as an official language (ISO-3 codes)
french_countries_iso <- c("BEN", "BFA", "BDI", "CMR", "CAF", 
                          "TCD", "COM", "COG", "COD", "DJI", 
                          "GNQ", "GAB", "GIN", "CIV", "MDG", 
                          "MLI", "NER", "RWA", "SEN", "SYC", 
                          "TGO", "BEL", "FRA", "LUX", "MCO", 
                          "CHE", "CAN", "HTI", "DMA", "LCA", 
                          "VUT")

# List of countries using Spanish as an official language (ISO-3 codes)
spanish_countries_iso <- c("ARG", "BOL", "CHL", "COL", "CRI", 
                           "CUB", "DOM", "ECU", "SLV", "GTM", 
                           "HND", "MEX", "NIC", "PAN", "PRY", 
                           "PER", "URY", "VEN", "ESP", "GNQ")


filter_by_iso <- function(data, iso_list) {
  data %>%
    filter(ISO %in% iso_list) # assuming your ISO column is named 'ISO'
}

# Create the filtered dataframes for each language group
chinese_data <- filter_by_iso(QI.data, chinese_countries_iso)
arabic_data <- filter_by_iso(QI.data, arabic_countries_iso)
russian_data <- filter_by_iso(QI.data, russian_countries_iso)
french_data <- filter_by_iso(QI.data, french_countries_iso)
spanish_data <- filter_by_iso(QI.data, spanish_countries_iso)


calculate_percentage_yes <- function(data) {
  total_entries <- nrow(data)
  if (total_entries == 0) {
    return(NA)  # Handle cases where there are no entries
  }
  yes_entries <- sum(data$Concordance == "Yes", na.rm = TRUE)
  percentage_yes <- (yes_entries / total_entries) * 100
  return(percentage_yes)
}

# Calculate the percentage for each language group dataframe
chinese_percentage_yes <- calculate_percentage_yes(chinese_data)
arabic_percentage_yes <- calculate_percentage_yes(arabic_data)
russian_percentage_yes <- calculate_percentage_yes(russian_data)
french_percentage_yes <- calculate_percentage_yes(french_data)
spanish_percentage_yes <- calculate_percentage_yes(spanish_data)

cat("Percentage of 'Yes' in Concordance for Chinese-speaking countries: ", chinese_percentage_yes, "%\n")
cat("Percentage of 'Yes' in Concordance for Arabic-speaking countries: ", arabic_percentage_yes, "%\n")
cat("Percentage of 'Yes' in Concordance for Russian-speaking countries: ", russian_percentage_yes, "%\n")
cat("Percentage of 'Yes' in Concordance for French-speaking countries: ", french_percentage_yes, "%\n")
cat("Percentage of 'Yes' in Concordance for Spanish-speaking countries: ", spanish_percentage_yes, "%\n")

QI.data <- read.csv("QIDataClean.csv")

QI.data <- QI.data %>%
  filter(!is.na(WHO_Region)) 

AMP.cited <- QI.data %>%
  filter(AMP.Cited != "")

no.major.diff.count <- AMP.cited %>%
  filter(Difference.after.excluding.AMP == "No major difference") %>%
  count()

