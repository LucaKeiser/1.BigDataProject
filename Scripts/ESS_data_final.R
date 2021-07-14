# set working directory
setwd(file.path("C:", "Users", "LucaK", "Desktop", "GitHub", "Big Data Analytics"))

# packages
library(data.table)
library(pryr)
library(tidyverse)
library(maps)



########################################### ESS-data #########################################

# ESS-data import with fread() -> memory is allocated in advanced -> much faster than read.csv
ESS <- fread("Data/ESS1-9e01_1.csv", verbose = TRUE)


# checking the data
# object_size(ESS)
# class(ESS)
# typeof(ESS)
# str(ESS)
# # View(ESS)
# # summary(ESS)


#################################################### create subset ########################################################
names(ESS)

ESS_small <- ESS %>% 
  select(c("cntry",
           "name",
           "cseqno",
           "idno",
           "essround",
           "dweight",
           "pspwght",
           "pweight",
           "anweight",
           "inwdd",
           "inwmm",
           "inwshh",
           "inwsmm",
           "inwehh",
           "inwemm",
           "inwtm",
           "imsmetn",
           "imdfetn",
           "impcntr",
           "imbgeco",
           "imueclt",
           "imwbcnt"))

# check
object_size(ESS_small)
summary(ESS_small)
mem_used()



############################################################## data cleaning ####################################################

# rename the countries with plyr
unique(ESS_small$cntry)

ESS_small$cntry <- plyr::revalue(ESS_small$cntry, c("AT" = "Austria",
                                                    "BE" = "Belgium",
                                                    "BG" = "Bulgaria",
                                                    "CH" = "Switzerland",
                                                    "CY" = "Cyprus",
                                                    "CZ" = "Czech Republic",
                                                    "DE" = "Germany",
                                                    "DK" = "Denmark",
                                                    "EE" = "Estonia",
                                                    "ES" = "Spain",
                                                    "FI" = "Finland",
                                                    "FR" = "France",
                                                    "GB" = "UK",
                                                    "GR" = "Greece",
                                                    "HR" = "Croatia",
                                                    "HU" = "Hungary",
                                                    "IE" = "Ireland",
                                                    "IL" = "Israel",
                                                    "IS" = "Iceland",
                                                    "IT" = "Italy",
                                                    "LT" = "Lithuania",
                                                    "LU" = "Luxembourg",
                                                    "LV" = "Latvia",
                                                    "NL" = "Netherlands",
                                                    "NO" = "Norway",
                                                    "PL" = "Poland",
                                                    "PT" = "Portugal",
                                                    "RU" = "Russia",
                                                    "SE" = "Sweden",
                                                    "SI" = "Slovenia",
                                                    "SK" = "Slovakia",
                                                    "TR" = "Turkey",
                                                    "UA" = "Ukraine"))

# exclude Russia, Turkey & Ukraine
ESS_countries_of_interest <- c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Spain", "Finland",
                               "France", "UK", "Greece", "Croatia", "Hungary", "Ireland", "Israel", "Iceland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands",
                               "Norway", "Poland", "Portugal", "Sweden", "Slovenia", "Slovakia")

ESS_small <- ESS_small %>% 
  filter(cntry %in% ESS_countries_of_interest)


# check
unique(ESS_small$cntry)
gc()
mem_used()



############################################# create a new data set with n_resp #############################################

# n_resp = number of respondents per country and round
n_resp <- ESS_small %>% 
  group_by(cntry, essround) %>% 
  summarise(n_resp = n())

# Note: not every country has participated in all the rounds! 
View(n_resp)

# merge the data
ESS_small <- ESS_small %>% 
  left_join(n_resp)

# check
# View(ESS_small)
# object_size(ESS_small)



################################################## information about the varibales ###############################################

# 1. ***imsmetn***: 
# Q: "Now, using this card, to what extent do you think [country] should allow people of the same race or ethnic group as most [country] people to come and live here?"
# 1 = "Allow many to come and live here" 
# 2 = "Allow some"
# 3 = "Allow a few"
# 4 = "Allow none"
# 7 or 77 = "Refusal" -> can be set to NA
# 8 or 88 = "Don't know" -> can be set to NA
# 9 or 99 = "No answer" -> can be set to NA

# 2. ***imdfetn***
# Q: "How about people of a different race or ethnic group from most [country] people?"
# 1 = "Allow many to come and live here"
# 2 = "Allow some"
# 3 = "Allow a few"
# 4 = "Allow none"
# 7 or 77 = "Refusal" -> can be set to NA
# 8 or 88 = "Don't know" -> can be set to NA
# 9 or 99 = "No answer" -> can be set to NA

# 3. ***impcntr***
# Q: "How about people from the poorer countries outside Europe?"
# 1 = "Allow many to come and live here"
# 2 = "Allow some"
# 3 = "Allow a few"
# 4 = "Allow none"
# 7 or 77 = "Refusal" -> can be set to NA
# 8 or 88 = "Don't know" -> can be set to NA
# 9 or 99 = "No answer" -> can be set to NA

# 4. ***imbgeco***
# Q: "Would you say it is generally bad or good for [country]'s economy that people come to live here from other countries?"
# from 0 (= "Bad for the economy") to 10 (= "Good for the economy")
# 77 = "Refusal" -> can be set to NA
# 88 = "Don't know" -> can be set to NA
# 99 = "No answer" -> can be set to NA

# 5. ***imueclt***
# Q: "And, using this card, would you say that [country]'s cultural life is generally undermined or enriched by people coming to live here from other countries?"
# from 0 (= "Cultural life undermined") to 10 (= "Cultural life enriched")
# 77 = "Refusal" -> can be set to NA
# 88 = "Don't know" -> can be set to NA
# 99 = "No answer" -> can be set to NA

# 6. ***imwbcnt***
# Q: "Immigrants make country worse or better place to live"
# from 0 (= "Worse place to live") to 10 (= "Better place to live")
# 77 = "Refusal" -> can be set to NA
# 88 = "Don't know" -> can be set to NA
# 99 = "No answer" -> can be set to NA

########################################################### assign NAs to the defined values & recode #######################################

# recode variables imsmetn, imdfetn & impcntr 
# assign NAs

ESS_small$imsmetn <-  recode(ESS_small$imsmetn, "1" = 4,
                             "2" = 3,
                             "3" = 2,
                             "4" = 1)
ESS_small$imdfetn <- recode(ESS_small$imdfetn, "1" = 4,
                            "2" = 3,
                            "3" = 2,
                            "4" = 1)

ESS_small$impcntr <- recode(ESS_small$impcntr, "1" = 4,
                            "2" = 3,
                            "3" = 2,
                            "4" = 1)

# assign NAs to the other variables (imbgeco, imueclt & imwbcnt)

ESS_small$imbgeco[ESS_small$imbgeco > 10] <- NA
ESS_small$imueclt[ESS_small$imueclt > 10] <- NA
ESS_small$imwbcnt[ESS_small$imwbcnt > 10] <- NA



###################################################### create subset with average values ######################################################

# all years
ESS_small_all_years <- ESS_small %>% 
  group_by(cntry, essround) %>% 
  summarise(avg_imsmetn = (sum(imsmetn, na.rm = TRUE)) / n_resp,
            avg_imdfetn = (sum(imdfetn, na.rm = TRUE)) / n_resp,
            avg_impcntr = (sum(impcntr, na.rm = TRUE)) / n_resp,
            avg_imbgeco = (sum(imbgeco, na.rm = TRUE)) / n_resp,
            avg_imueclt = (sum(imueclt, na.rm = TRUE)) / n_resp,
            avg_imwbcnt = (sum(imwbcnt, na.rm = TRUE)) / n_resp) 


# get rid of duplicates...
ESS_small_all_years <- unique(setDT(ESS_small_all_years))

# check
# gc()
# mem_used()

######################################################### create migration-index ##############################################################

# The migration index is composed as follows:
#   
# In a first step, the mean values of the migration variables (imsmetn, imdfetn, impcntr, imbgeco, imueclt & imwbcntin) per year were calculated.
# In a next step, the calculated mean values were summed up and divided by the number of variables. 
# Since the response categories of the variables imsmetn, imdfetn & impcntr are only between 1 and 4, they were weighted with a factor of 2.5. 
# Finally, the variation range was normalised between 0 and 1.

# all years
ESS_small_all_years <- ESS_small_all_years %>% 
  group_by(cntry, essround) %>% 
  mutate(migration_index = c(((2.5 * avg_imsmetn) + (2.5 * avg_imdfetn) + (2.5 * avg_impcntr) + avg_imbgeco + avg_imueclt + avg_imwbcnt) / 6),
         migration_index_norm = ((migration_index - 1.25) / (10 - 1.25)))



####################################### create data for mapping and merge data sets #####################################

# creating a subset of the ESS-countries
ESS_countries_map <- map_data("world", region = unique(ESS_small_all_years$cntry)) %>% 
  rename(cntry = region)

# merging the data sets
ESS_small_all_years <- left_join(ESS_small_all_years, ESS_countries_map)

# creating the position for the country-labels -> avg of long & avg of lat
country_labels_all_years <- ESS_small_all_years %>% 
  group_by(cntry) %>%
  summarise(long = mean(long), lat = mean(lat))



########################################################### create maps #####################################################


# parallel processing with doSNOW
library(doSNOW)
ncores <- parallel::detectCores()
ctemp <- makeCluster(ncores)
registerDoSNOW(ctemp)


### all years ###

# plot
attitude_all_years <- ggplot() +
  geom_polygon(data = ESS_small_all_years,
               aes(x = long, y = lat, group = group, fill = migration_index_norm), color = "white") +         
  theme_light(base_size = 15, base_family = "serif") +
  #  geom_text(data = country_labels_all_years, aes(x = long, y = lat, label = cntry), size = 2.5, color = "black") +
  scale_fill_gradient2(midpoint = 0.5, low = "royalblue1", mid = "forestgreen", high = "gold1") + 
  labs(
    title = "Attitudes from 2002 to 2018",
    x = "Longitude",
    y = "Latitude",
    fill = "Migration-Index"
  ) +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 40)) + # zoom in
  facet_wrap(~ essround)


# generate plot
attitude_all_years


### pre 2015 ###


# create the mean of the index first
ESS_small_2002_2014  <- ESS_small_all_years %>%
  filter(essround %in% c(1:7))

ESS_small_2002_2014 <- data.table(ESS_small_2002_2014)
ESS_small_2002_2014 <- ESS_small_2002_2014[,list(mean_migration_index_norm = mean(migration_index_norm)), by = c("cntry")]


# merge data sets
ESS_small_2002_2014 <- ESS_countries_map %>% 
  left_join(ESS_small_2002_2014, by = c("cntry")) 


# plot
attitude_2002_2014 <- ggplot() +
  geom_polygon(data = ESS_small_2002_2014,
               aes(x = long, y = lat, group = group, fill = mean_migration_index_norm), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels_all_years, aes(x = long, y = lat, label = cntry), size = 2.5, color = "black") +
  scale_fill_gradient2(midpoint = 0.5, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  labs(
    title = "avg. Attitude-Index from 2002 to 2014",
    x = "Longitude",
    y = "Latitude",
    fill = "Migration-Index"
  ) +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 40)) # zoom in


# generate plot
attitude_2002_2014



### after 2015 ###

# create the mean of the index first
ESS_small_2016_2018  <- ESS_small_all_years %>%
  filter(essround %in% c(8:9))


ESS_small_2016_2018 <- data.table(ESS_small_2016_2018)
ESS_small_2016_2018 <- ESS_small_2016_2018[,list(mean_migration_index_norm = mean(migration_index_norm)), by = c("cntry")]


# merge the data sets
ESS_small_2016_2018 <- ESS_countries_map %>% 
  left_join(ESS_small_2016_2018, by = c("cntry")) 


# plot
attitude_2016_2018 <- ggplot() +
  geom_polygon(data = ESS_small_2016_2018,
               aes(x = long, y = lat, group = group, fill = mean_migration_index_norm), color = "white") +
  theme_light(base_size = 15, base_family = "serif") +
  geom_text(data = country_labels_all_years, aes(x = long, y = lat, label = cntry), size = 2.5, color = "black") +
  scale_fill_gradient2(midpoint = 0.5, low = "royalblue1", mid = "forestgreen", high = "gold1") +
  labs(
    title = "avg. Attitude-Index from 2016 to 2018",
    x = "Longitude",
    y = "Latitude",
    fill = "Migration-Index"
  ) +
  coord_fixed(5) +
  coord_cartesian(xlim = c(-25, 40)) # zoom in


# generate plot
attitude_2016_2018


# stop the doSNOW-cluster
stopCluster(cl = ctemp)

