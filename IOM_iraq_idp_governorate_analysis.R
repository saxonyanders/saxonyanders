library(tidyverse)

idp_raw <- read_csv("iom_idp_all.csv")
ret_raw <- read_csv("iom_returnee_all.csv")

print(idp_raw)
print(return_raw)

#set up destination tables

all_periods <- c("Pre-June 2014", "June-July 2014", "August 2014", "Post September 2014",
                 "Post April 2015", "Post March 2016", "Post October 2016", "July 2017",
                 "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022", "Jan 2023", "Jan 2024")

idp_places <- idp_raw %>%
  select(round_number, round_date, count_unit, place_id, 
         Governorate, District,
         idp_households, idp_individuals, all_of(all_periods))

idp_places <- idp_places %>%
  distinct(round_number, place_id, .keep_all = TRUE)

idp_places <- idp_places %>%
  mutate(multiplier = idp_individuals / idp_households)

idp_places <- idp_places %>%
  mutate(across(all_of(all_periods),
                ~ if_else(count_unit == "individuals", .x / multiplier, .x)))

ret_places <- ret_raw %>%
  select(round_number, round_date, count_unit, place_id,
         Governorate, District,
         returnee_households, returnee_individuals, all_of(all_periods))

ret_places <- ret_places %>%
  distinct(round_number, place_id, .keep_all = TRUE)

ret_places <- ret_places %>%
  mutate(multiplier = returnee_individuals / returnee_households)

ret_places <- ret_places %>%
  mutate(across(all_of(all_periods),
                ~ if_else(count_unit == "individuals", .x / multiplier, .x)))


idp_places <- idp_places %>%
  mutate(region = if_else(Governorate %in% c("Dahuk", "Erbil", "Sulaymaniyah"),
                          "KRI", "Federal Iraq"))

ret_places <- ret_places %>%
  mutate(region = if_else(Governorate %in% c("Dahuk", "Erbil", "Sulaymaniyah"),
                          "KRI", "Federal Iraq"))
    

