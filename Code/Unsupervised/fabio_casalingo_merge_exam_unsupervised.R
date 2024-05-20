library(readxl)
library(tidyverse)
library(readxl)
library(countrycode)

#Importing datas
data_energy <- read.csv("Datasets/Datasets_unsupervised/energy/Electricity_Production_By_Source.csv")
data_pop <- read.csv("Datasets/Datasets_unsupervised/pop_country/pop.csv")
data_c02 <- read.csv("Datasets/Datasets_unsupervised/c02_best/c02_2.csv")
data_forest <- read.csv("Datasets/Datasets_unsupervised/forest/forest.csv")
data_ans <- read.csv("Datasets/Datasets_unsupervised/adjasted_net_seving/adjasted_net_seving.csv")
data_air <- read.csv("Datasets/Datasets_unsupervised/air_q/AQI/AQI.csv")

#filter for 2020
data_c02_2020 <- data_c02[, c("Country.Code", "X2020")]
data_energy_2020 <- subset(data_energy, Year == "2020")
data_pop_2020 <- data_pop[, c("Country.Code", "X2020")]
data_forest_2020 <- data_forest[, c("Country.Code", "X2020")]
data_ans_2020 <- data_ans[, c("Country.Code", "X2020")]

#filter air datas
data_air_2020 <- data_air %>%  
  filter(Country != "") %>%
  select(-c(2,4,6,8,10,12:14)) %>%
  group_by(Country) %>% 
  summarise_at(vars(AQI.Value, CO.AQI.Value, Ozone.AQI.Value, NO2.AQI.Value, PM2.5.AQI.Value), mean, na.rm = TRUE)

iso3 <- function(dataset, column) {
  dataset$ISO3 <- countrycode(dataset[[column]], "country.name", "iso3c")
  return(dataset)
}

#Rename the column
data_air_2020 <- iso3(data_air_2020, "Country")
data_air_2020 <- data_air_2020[, -which(names(data_air_2020) == "Country")]

#Merges
data_2020 <- merge(data_c02_2020, data_energy_2020, by.x = "Country.Code", by.y = "Code")
data_2020 <- merge(data_2020, data_pop_2020, by= "Country.Code")
data_2020 <- merge(data_2020, data_forest_2020, by= "Country.Code")
data_2020 <- merge(data_2020, data_ans_2020, by= "Country.Code")
data_2020 <- merge(data_2020, data_air_2020, by.x = "Country.Code", by.y = "ISO3")

data_2020 <- data_2020[, -which(names(data_2020) == "Entity")]

#rename column
names(data_2020)[names(data_2020) == "X2020.x"] <- "C02_emitions"
names(data_2020)[names(data_2020) == "X2020.y"] <- "Population"
names(data_2020)[names(data_2020) == "X2020.x.1"] <- "Forest"
names(data_2020)[names(data_2020) == "X2020.y.1"] <- "Annual_Net_Saving"

#write the dataset into csv
write.csv(data_2020, "Datasets/Datasets_unsupervised/merged_un.csv", row.names = FALSE)

