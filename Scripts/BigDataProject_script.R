# packages needed

install.packages("tidyverse")
install.packages("pryr")
install.packages("data.table")
install.packages("ff")
install.packages("ffbase")
install.packages("maps")
install.packages("mapdata")
install.packages("foreign")
install.packages("haven")
install.packages("plyr")
install.packages("doSNOW")



# countries in the dataset
ESS_countries <- c("Albania", 
                   "Austria",
                   "Belgium",
                   "Bulgaria",
                   "Croatia",
                   "Cyprus",
                   "Czech Republic",
                   "Denmark",
                   "Estonia",
                   "Finland",
                   "France",
                   "Germany",
                   "Greece",
                   "Hungary",
                   "Italy",
                   "Latvia",
                   "Luxembourg",
                   "Netherlands",
                   "Norway",
                   "Poland",
                   "Portugal",
                   "Romania",
                   "Serbia",
                   "Slovakia",
                   "Slovenia",
                   "Spain",
                   "Sweden",
                   "Switzerland",
                   "UK")

# creating a subset of the ESS-countries
ESS_countries_maps <- map_data("world", region = ESS_countries)

# creating the position for the country-labels -> avg of long & avg of lat
ESS_countries_lab <- ESS_countries_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))