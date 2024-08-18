library(tidyr)
library(readr)
library(dplyr)
library(countrycode)

#load data 
ai.raw <- read.csv("AI_Vaxx_QAQC.csv")
#WHO Regions
who.raw <- read.csv("WHO_Regions_AI.csv")
#add ISOs
ai.raw$ISO <- countrycode(sourcevar = ai.raw$Country, origin = "country.name", destination = "iso3c")

#add lost countries
lost.countries <- data.frame(Country.Name = c("Cote d'Ivoire", "Liechtenstein", "Niue"), 
                             ISO3 = c("CIV", "LIE", "NIU"), 
                             WHO_Region = c("AFRO", "EURO", "WPRO"))

#add lost countries to the WHO dataset
who.raw <- rbind(who.raw, lost.countries)

#Put together the military dataset and the who regions
ai.data <- left_join(ai.raw, who.raw, by = c("ISO" = "ISO3"))

#write csv
write.csv(ai.data, "/Users/ciaraweets/Dropbox/R Projects/AI_policy_epi/AIDataClean.csv", row.names = FALSE)
