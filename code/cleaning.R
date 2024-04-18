library(dplyr)
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)
library(readr)
library(Hmisc)
library(tibble)
library(stats)
library(janitor)


# import data
rider_23 <- read.csv("../data/ridership_2023.csv")
rider_19 <- read.csv("../data/ridership_2019.csv")
rider_covid <- read.csv("../data/ridership_covid.csv")
stops <- read.csv("../data/stops.csv")
first_schools <- st_read("../data/shp_struc_school_program_locs/")
second_schools <- st_read("../data/shp_society_post_second_enroll/")

# data cleaning
date <- c("dtBookingStart", "dtWeekStart", "dtDate", "dtMonthYear")
rider_23 <- rider_23 %>% mutate(across(all_of(date), as.Date))
rider_19 <- rider_19 %>% mutate(across(all_of(date), as.Date))
rider_covid <- rider_covid %>% mutate(across(all_of(date), as.Date))
stops_sf_county <- st_as_sf(stops, coords = c("lon", "lat"), crs = 4269) %>%
  filter(County %in% c("Ramsey", "Hennepin"))

# census data:  get variables for Ramsey and Hennepin area
# There is something wrong with the website, will figure that out later, using year= 2021 for the testing
census2023 <- tidycensus::get_acs(
  year = 2022,
  state = "MN",
  geography = "tract",
  variables = c("B01003_001", "B19013_001","B02001_002","B02001_003","B02001_004","B02001_005","B02001_006","B02001_007","B02001_008"),
  output = "wide",
  geometry = TRUE
) %>%
  filter(word(NAME, 4) %in% c("Ramsey", "Hennepin")) %>% # only filter by the 4th words which represents counties
  mutate(
    tract = word(NAME, 3), # ie.  Census Tract 508.24, Anoka County, Minnesota, only need "508.24"
    tract = str_remove(tract, ","),
    county = word(NAME, 4)
  ) %>%
  select(-NAME) %>%
  rename(
    "population" = "B01003_001E",
    "medianIncome" = "B19013_001E",
    "White Alone" = "B02001_002E",
    "Black or African American Alone" = "B02001_003E",
    "American Indian and Alaska Native Alone" = "B02001_004E",
    "Asian Alone" = "B02001_005E",
    "Native Hawaiian and Other Pacific Islander Alone" = "B02001_006E",
    "Some Other Race Alone" = "B02001_007E",
    "Two or More Races" = "B02001_008E"
    
  ) %>%
  select(-contains("_"))

name <- names(census2023)[c(-1)]



########################################## Education ################################################

#joining the bus stop data with the census2023 dataset
stops_census_join <- st_join(census2023, stops_sf_county)

#creating the number of bus stops subsetted by the tract, including population, medIncome, Races
stops_census_join_summ <- stops_census_join %>% 
  group_by(tract, population, medianIncome, `White Alone`, `Black or African American Alone`, `Asian Alone`, `American Indian and Alaska Native Alone`, `Native Hawaiian and Other Pacific Islander Alone`) %>% 
  summarise(number_of_stops = n_distinct(StopID))

#joining the education data to the main dataset
first_schools <- first_schools %>% 
  st_transform(crs = st_crs(stops_census_join_summ))
second_schools <- second_schools %>% 
  st_transform(crs = st_crs(stops_census_join_summ))

education_pt1 <- st_join(stops_census_join_summ, first_schools) %>% 
  filter(COUNTYNAME %in% c("Ramsey", "Hennepin"))

education_final <- st_join(education_pt1, second_schools) %>% 
  filter(COUNTYNAME %in% c("Ramsey", "Hennepin"))
education_final <- education_final %>% 
  select(tract, geometry, GISNAME, GISADDR, LOCDISTNAM, COUNTYNAME, number_of_stops)
duplicate_rows3 <- duplicated(education_final$GISADDR)
unique_education <- education_final[!duplicate_rows3, ]


# Number of schools per tract & then reference to the number of stops
schools_stops <- unique_education %>% 
  group_by(tract, number_of_stops) %>% 
  summarise(number_school = n_distinct(GISNAME))

## COUNT BY THE NUMBER OF SCHOOLS IN THE TRACT AND THEN THE NUMBER OF STOPS ##


##################### used_ridership #####################
used_ridership_all <- rbind(rider_23,rider_19,rider_covid) %>% select(c(3,5,6,8,10,34,35,36,40)) 
weekends_weekdays <- used_ridership_all %>% 
  filter(!(Schedule %in% c("Holiday","Reduced")))%>% 
  mutate(Schedule = case_when(Schedule %in% c("Saturday", "Sunday") ~ "Weekend", Schedule == "Weekday"~ "Weekday")) %>% 
  group_by(nYear, Schedule) %>%
  summarise(Total_Riders = sum(Total_Riders,na.rm = TRUE))
