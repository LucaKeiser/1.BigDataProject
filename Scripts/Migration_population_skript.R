# set working directory
setwd(file.path("C:", "Users", "LucaK", "Desktop", "GitHub", "Big Data Analytics"))

# packages
library(data.table)
library(pryr)
library(tidyverse)
library(maps)
library(mapdata)

# Countries in the ESS-data set
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
                   "Iceland",
                   "Israel",
                   "Italy",
                   "Kosovo",
                   "Latvia",
                   "Lithuania",
                   "Luxembourg",
                   "Montenegro",
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
                   "Turkey",
                   "Ukraine",
                   "UK",
                   "Russia")





################################################ import and clean inflow-data ###############################################
# import inflow data set
inflow <- fread("Data/inflow.csv")

# cleaning
inflow <-  inflow %>% 
  pivot_wider(names_from = VAR, values_from = Value) %>% 
  rename(inflow_population = B11,
         inflow_asylum = B13,
         inflow_worker = B21)

# revalue with plyr
inflow$Country <- plyr::revalue(inflow$Country, c("United Kingdom" = "UK",
                                                  "Slovak Republic" = "Slovakia"))

# check
unique(inflow$Country)
mem_used()

# replace NA with 0
inflow[is.na(inflow)] <- c(0)

# creating a subset
inflow_s <- inflow %>% 
  select(c(Country, Year, inflow_population, inflow_asylum, inflow_worker)) %>% 
  filter(Country %in% ESS_countries) %>% 
  group_by(Country, Year) %>% 
  summarise(total_foreign_inflow = sum(inflow_population),
            total_asylum_inflow = sum(inflow_asylum),
            total_worker_inflow = sum(inflow_worker)) %>% 
  mutate(total_inflow = total_foreign_inflow + total_asylum_inflow + total_worker_inflow)



########################################### import & clean population-data ###############################################
population <- fread("Data/population.csv", verbose = TRUE)

# cleaning the data
# revalue with plyr
population$Country <- plyr::revalue(population$Country, c("United Kingdom" = "UK",
                                                          "Slovak Republic" = "Slovakia"))

# rename
population <- population %>% 
  rename(Year = Time)

# check
unique(population$Country)
mem_used()

# creating a subset
population_s <- population %>% 
  select(c(Country, Year, Value)) %>% 
  filter(Country %in% ESS_countries) %>% 
  group_by(Country, Year)



##################################################### merging the data sets ########################################################
population_inflow <- inflow_s %>% 
  left_join(population_s, by = c("Country", "Year")) %>% 
  rename(population_size = Value) %>% 
  relocate(Country, Year, population_size)

# creating a new variables -> per 1000 inhabitants
population_inflow <- population_inflow %>% 
  group_by(Country, Year) %>% 
  mutate(foreign_inflow_1000 = (total_foreign_inflow/population_size) * 1000,
         asylum_inflow_1000 = (total_asylum_inflow/population_size) * 1000,
         worker_inflow_1000 = (total_worker_inflow/population_size) * 1000,
         total_inflow_1000 = (total_inflow/population_size) * 1000) 

# first overlook
ggplot(population_inflow, aes(Year, total_inflow_1000)) +
  geom_line(aes(color = Country))



######################################################### mapping the data ####################################################################
# creating a subset of the ESS-countries
ESS_country_maps <- map_data("world", region = ESS_countries) %>% 
  rename(Country = region)


# merging the data sets
inflow_map_data <-  ESS_country_maps %>% 
  left_join(population_inflow)

inflow_map_data[is.na(inflow_map_data)] <- 0

# creating the position for the country-labels -> avg of long & avg of lat
country_labels <- inflow_map_data %>%
  group_by(Country) %>%
  summarise(long = mean(long), lat = mean(lat))



###############################################***FROM 2001 TO 2006***##########################################################################
inflow_map_data_01_06 <- inflow_map_data %>% 
  filter(Year %in% c(2001:2006))


############################## 1. plot -> Inflow of Foreign Population per 1000 inhabitants ####################################################
ggplot() +
  geom_polygon(data = inflow_map_data_01_06, aes(x = long, y = lat, group = group, fill = foreign_inflow_1000), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Foreign Population",
    subtitle = "From 2001 to 2006",
    x = "Longitude",
    y = "Latitude",
    fill = "Foreign Population Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 30, low = "royalblue1", mid = "forestgreen", high = "gold1") + 
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################ 2. plot -> Inflow of Asylum Seekers per 1000 inhabitants #####################################################

ggplot() +
  geom_polygon(data = inflow_map_data_01_06, aes(x = long, y = lat, group = group, fill = asylum_inflow_1000), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Asylum Seekers",
    subtitle = "From 2001 to 2006",
    x = "Longitude",
    y = "Latitude",
    fill = "Asylum Seeker Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 4.5, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################ 3. plot -> Inflow of Foreign Wokers per 1000 inhabitants ######################################################

ggplot() +
  geom_polygon(data = filter(inflow_map_data_01_06, Country != "Luxembourg"),                                 # note: Luxembourg is excluded here
               aes(x = long, y = lat, group = group, fill = worker_inflow_1000), color = "white") +           # worker_inflow_1000 > 100 -> "outliner"
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Foreign Workers",
    subtitle = "From 2001 to 2006",
    x = "Longitude",
    y = "Latitude",
    fill = "Inflow of Foreign Workers \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 20, low = "royalblue1", mid = "forestgreen", high = "gold1") + 
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)




################################## 4. plot -> Total Inflow per 1000 inhabitants ################################################################

ggplot() +
  geom_polygon(data = filter(inflow_map_data_01_06, Country != "Luxembourg"),                                 # note: Luxembourg is excluded here
               aes(x = long, y = lat, group = group, fill = total_inflow_1000), color = "white") +            # total_inflow_1000 > 150 -> "outliner"
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Total Inflow",
    subtitle = "From 2001 to 2006",
    x = "Longitude",
    y = "Latitude",
    fill = "Total Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 30, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)






###############################################***FROM 2007 TO 2012***##########################################################################
inflow_map_data_07_12 <- inflow_map_data %>% 
  filter(Year %in% c(2007:2012))


############################## 1. plot -> Inflow of Foreign Population per 1000 inhabitants ####################################################
ggplot() +
  geom_polygon(data = inflow_map_data_07_12, aes(x = long, y = lat, group = group, fill = foreign_inflow_1000), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Foreign Population",
    subtitle = "From 2007 to 2012",
    x = "Longitude",
    y = "Latitude",
    fill = "Foreign Population Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 35, low = "royalblue1", mid = "forestgreen", high = "gold1") + 
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################ 2. plot -> Inflow of Asylum Seekers per 1000 inhabitants #####################################################

ggplot() +
  geom_polygon(data = inflow_map_data_07_12, aes(x = long, y = lat, group = group, fill = asylum_inflow_1000), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Asylum Seekers",
    subtitle = "From 2007 to 2012",
    x = "Longitude",
    y = "Latitude",
    fill = "Asylum Seeker Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 4.5, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################ 3. plot -> Inflow of Foreign Wokers per 1000 inhabitants ######################################################

ggplot() +
  geom_polygon(data = filter(inflow_map_data_07_12, Country != "Luxembourg"),                                 # note: Luxembourg is excluded here
               aes(x = long, y = lat, group = group, fill = worker_inflow_1000), color = "white") +           # worker_inflow_1000 > 127 -> "outliner"
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Foreign Workers",
    subtitle = "From 2007 to 2012",
    x = "Longitude",
    y = "Latitude",
    fill = "Inflow of Foreign Workers \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 35, low = "royalblue1", mid = "forestgreen", high = "gold1") +             # note: midpoint is set to 35
  coord_fixed(5) +                                                                                           # for better visualisation
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################## 4. plot -> Total Inflow per 1000 inhabitants ################################################################

ggplot() +
  geom_polygon(data = filter(inflow_map_data_07_12, Country != "Luxembourg"),                                 # note: Luxembourg is excluded here
               aes(x = long, y = lat, group = group, fill = total_inflow_1000), color = "white") +            # total_inflow_1000 > 190 -> "outliner"
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Total Inflow",
    subtitle = "From 2007 to 2012",
    x = "Longitude",
    y = "Latitude",
    fill = "Total Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 45, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)





###############################################***FROM 2013 TO 2018***##########################################################################
inflow_map_data_13_18 <- inflow_map_data %>% 
  filter(Year %in% c(2013:2018))


############################## 1. plot -> Inflow of Foreign Population per 1000 inhabitants ####################################################
ggplot() +
  geom_polygon(data = inflow_map_data_13_18, aes(x = long, y = lat, group = group, fill = foreign_inflow_1000), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Foreign Population",
    subtitle = "From 2013 to 2018",
    x = "Longitude",
    y = "Latitude",
    fill = "Foreign Population Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 40, low = "royalblue1", mid = "forestgreen", high = "gold1") + 
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################ 2. plot -> Inflow of Asylum Seekers per 1000 inhabitants #####################################################

ggplot() +
  geom_polygon(data = inflow_map_data_13_18, aes(x = long, y = lat, group = group, fill = asylum_inflow_1000), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Asylum Seekers",
    subtitle = "From 2013 to 2018",
    x = "Longitude",
    y = "Latitude",
    fill = "Asylum Seeker Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 15, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################ 3. plot -> Inflow of Foreign Wokers per 1000 inhabitants ######################################################

ggplot() +
  geom_polygon(data = inflow_map_data_13_18, aes(x = long, y = lat, group = group, fill = worker_inflow_1000), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Inflow of Foreign Workers",
    subtitle = "From 2013 to 2018",
    x = "Longitude",
    y = "Latitude",
    fill = "Inflow of Foreign Workers \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 35, low = "royalblue1", mid = "forestgreen", high = "gold1") +             # note: midpoint is set to 35
  coord_fixed(5) +                                                                                           # for better visualisation
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)



################################## 4. plot -> Total Inflow per 1000 inhabitants ################################################################

ggplot() +
  geom_polygon(data = filter(inflow_map_data_13_18, Country != "Luxembourg"),                                 # note: Luxembourg is excluded here
               aes(x = long, y = lat, group = group, fill = total_inflow_1000), color = "white") +            # total_inflow_1000 > 190 -> "outliner"
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels, aes(x = long, y = lat, label = Country), size = 2.5, color = "black") +
  labs(
    title = "Total Inflow",
    subtitle = "From 2013 to 2018",
    x = "Longitude",
    y = "Latitude",
    fill = "Total Inflow \n(per 1000 inhabitants)"
  ) +
  scale_fill_gradient2(midpoint = 45, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 50)) + # Russia is too big -> zoom in
  facet_wrap(~Year)


