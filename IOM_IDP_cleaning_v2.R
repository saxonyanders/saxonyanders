## ---------------------------------------------------------------------------
## IOM DTM Iraq - IDP Master Lists, 19 rounds from 2014 to 2024
## Produces ONE long-format CSV: one row per location per governorate of origin,
## with displacement-period columns retained in wide format.
## Shelter columns are dropped.
## Workbooks are expected in a subfolder called "IDP_sheets".
## ---------------------------------------------------------------------------

library(tidyverse)  # loads dplyr, tidyr, purrr, stringr, readr in one go
library(readxl)     # reads .xlsx and .xls files


## ---------------------------------------------------------------------------
## SECTION 1: governorate name crosswalk
## ---------------------------------------------------------------------------

# maps every governorate spelling variant found across rounds to one canonical name
governorate_lookup <- c(
  "Anbar" = "Anbar",                      # canonical form, used rounds 5 to 131
  "Al Anbar" = "Anbar",                   # round 134 renamed this
  "Babylon" = "Babylon",                  # canonical form
  "Babil" = "Babylon",                    # round 134 variant
  "Baghdad" = "Baghdad",                  # stable across all rounds
  "Basrah" = "Basrah",                    # canonical form
  "Al Basrah" = "Basrah",                 # round 134 variant
  "Dahuk" = "Dahuk",                      # canonical form
  "Duhok" = "Dahuk",                      # round 134 variant
  "Diyala" = "Diyala",                    # stable across all rounds
  "Erbil" = "Erbil",                      # stable across all rounds
  "Kerbala" = "Kerbala",                  # stable across all rounds
  "Kirkuk" = "Kirkuk",                    # stable across all rounds
  "Missan" = "Missan",                    # stable across all rounds
  "Muthanna" = "Muthanna",                # canonical form
  "Al Muthanna" = "Muthanna",             # round 134 variant
  "Najaf" = "Najaf",                      # canonical form
  "Al Najaf" = "Najaf",                   # round 134 variant
  "Ninewa" = "Ninewa",                    # stable across all rounds
  "Qadissiya" = "Qadissiya",              # canonical form
  "Al Diwaniya" = "Qadissiya",            # round 134 renamed this governorate entirely
  "Salah al-Din" = "Salah al-Din",        # canonical form
  "Salah Al-Din" = "Salah al-Din",        # round 134 capitalisation variant
  "Salahal Din" = "Salah al-Din",         # round 101 variant (missing space)
  "Sulaymaniyah" = "Sulaymaniyah",        # canonical form
  "Al Sulaymaniyah" = "Sulaymaniyah",     # round 134 variant
  "Thi-Qar" = "Thi-Qar",                  # canonical form
  "Thi Qar" = "Thi-Qar",                  # round 101 variant (missing hyphen)
  "Wassit" = "Wassit"                     # stable across all rounds
)

# the 18 canonical governorate names, used as the pivot target
all_governorates <- unique(unname(governorate_lookup))


## ---------------------------------------------------------------------------
## SECTION 2: displacement period crosswalk
## ---------------------------------------------------------------------------

# maps every period heading and value variant found across rounds to one canonical label
period_lookup <- c(
  "Pre-June 14" = "Pre-June 2014",                                    # early rounds
  "Pre-June14" = "Pre-June 2014",                                     # no-space variant
  "Pre-June" = "Pre-June 2014",                                       # round 8 value
  "Pre-June14 Wave" = "Pre-June 2014",                                # rounds 32 and 36
  "Pre June14 Period" = "Pre-June 2014",                              # mid-series variant
  "Pre-June14 Period" = "Pre-June 2014",                              # hyphenated variant
  "Pre June14 Period of displacement" = "Pre-June 2014",              # round 101
  "June-July 14" = "June-July 2014",                                  # early rounds
  "June-July14" = "June-July 2014",                                   # no-space variant
  "June / July" = "June-July 2014",                                   # round 8 value
  "June-July14 Wave" = "June-July 2014",                              # rounds 32 and 36
  "June July14 Period" = "June-July 2014",                            # mid-series variant
  "June-July14 Period" = "June-July 2014",                            # hyphenated variant
  "June July14 Period of displacement" = "June-July 2014",            # round 101
  "August 14" = "August 2014",                                        # early rounds
  "August14" = "August 2014",                                         # no-space variant
  "August14 Wave" = "August 2014",                                    # rounds 32 and 36
  "August14 Period" = "August 2014",                                  # mid-series variant
  "August14 Period of displacement" = "August 2014",                  # round 101
  "Post September 14" = "Post September 2014",                        # early rounds
  "Post September14" = "Post September 2014",                         # no-space variant
  "Post-August" = "Post September 2014",                              # round 8 value, per your decision
  "Post September14 Wave" = "Post September 2014",                    # rounds 32 and 36
  "Post September 14 Period" = "Post September 2014",                 # mid-series variant
  "Post September14 Period" = "Post September 2014",                  # no-space variant
  "Post September 14 Period of displacement" = "Post September 2014", # round 101
  "Post April15" = "Post April 2015",                                 # early variant
  "Post April15 Wave" = "Post April 2015",                            # rounds 32 and 36
  "Post April15 Period" = "Post April 2015",                          # mid-series variant
  "Post April15 Period of displacement" = "Post April 2015",          # round 101
  "Post March16" = "Post March 2016",                                 # early variant
  "Post March 16 Period" = "Post March 2016",                         # mid-series variant
  "Post Mar16 Period" = "Post March 2016",                            # abbreviated variant
  "Post March 16 Period of displacement" = "Post March 2016",         # round 101
  "Post 17 October16" = "Post October 2016",                          # early variant
  "Post 17 October 16 Period" = "Post October 2016",                  # mid-series variant
  "Post 17 October 16 Period of displacement" = "Post October 2016",  # round 101
  "July 17" = "July 2017",                                            # rounds 87 onwards
  "July 17 Period of displacement" = "July 2017",                     # round 101
  "Jan 19" = "Jan 2019",                                              # rounds 112 to 131
  "Jan 2019" = "Jan 2019",                                            # round 134
  "Jan 20" = "Jan 2020",                                              # rounds 119 to 131
  "Jan 2020" = "Jan 2020",                                            # round 134
  "Jan2021" = "Jan 2021",                                             # no-space variant
  "Jan 2021" = "Jan 2021",                                            # spaced variant
  "Jan2022" = "Jan 2022",                                             # no-space variant
  "Jan 2022" = "Jan 2022",                                            # spaced variant
  "Jan2023" = "Jan 2023",                                             # no-space variant
  "Jan 2023" = "Jan 2023",                                            # spaced variant
  "Jan2024" = "Jan 2024",                                             # no-space variant
  "Jan 2024" = "Jan 2024"                                             # spaced variant
)

# the 14 canonical period labels, in chronological order
all_periods <- c("Pre-June 2014", "June-July 2014", "August 2014", "Post September 2014",
                 "Post April 2015", "Post March 2016", "Post October 2016", "July 2017",
                 "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022", "Jan 2023", "Jan 2024")


## ---------------------------------------------------------------------------
## SECTION 3: identifier, geography and count column crosswalk
## ---------------------------------------------------------------------------

# maps identifier, geography and count column variants to one consistent set of names
identifier_lookup <- c(
  "Place ID" = "place_id",                              # rounds 12 to 36, and round 101
  "Place id" = "place_id",                              # rounds 51 onwards
  "Location Name" = "location_name",                    # rounds 12 to 36
  "Location name" = "location_name",                    # mid-series variant
  "Location name in English" = "location_name",         # rounds 51 onwards
  "Location_name" = "location_name",                    # round 101 underscore variant
  "Place" = "location_name",                            # rounds 5 and 8
  "Arabic Name" = "location_name_arabic",               # rounds 12 to 36
  "Arabic name" = "location_name_arabic",               # mid-series variant
  "Location name in Arabic" = "location_name_arabic",   # rounds 51 onwards
  "Arabic_name" = "location_name_arabic",               # round 101 underscore variant
  "OCHA Adm1 PCode" = "ocha_admin_1",                   # rounds 12 to 19
  "OCHA Admin 1" = "ocha_admin_1",                      # rounds 32 to 36
  "OCHA admin 1" = "ocha_admin_1",                      # rounds 51 onwards
  "OCHA ADM2 PCode" = "ocha_admin_2",                   # rounds 12 to 19
  "OCHA Admin 2" = "ocha_admin_2",                      # rounds 32 to 36
  "OCHA admin 2" = "ocha_admin_2",                      # rounds 51 onwards
  "OCHA_PCode" = "ocha_pcode",                          # rounds 12 to 19
  "OCHA PCode" = "ocha_pcode",                          # rounds 32 onwards
  "IDPs Families" = "idp_households",                   # rounds 12 to 32
  "IDPs families" = "idp_households",                   # lowercase variant
  "Families" = "idp_households",                        # rounds 36 to 101
  "ID Pfamilies" = "idp_households",                    # round 64, mangled by tatweel removal
  "IDPfamilies" = "idp_households",                     # alternative mangling, kept as a safeguard
  "Households" = "idp_households",                      # rounds 119 onwards
  "Master Families" = "idp_households",                 # rounds 5 and 8
  "IDPs Individuals" = "idp_individuals",               # rounds 12 to 32
  "IDPs individuals" = "idp_individuals",               # lowercase variant
  "Individuals" = "idp_individuals",                    # rounds 36 onwards
  "ID Pindividuals" = "idp_individuals",                # round 64, mangled by tatweel removal
  "IDPindividuals" = "idp_individuals",                 # alternative mangling, kept as a safeguard
  "Host Families" = "host_families",                    # rounds 19 to 32
  "Host families" = "host_families",                    # mid-series variant
  "Hostfamilies" = "host_families"                      # round 64 variant
)

# code fields that must be read as text, since leading zeros vary between rounds
code_columns <- c("place_id", "Place Code", "District Code", "Location ID",
                  "ocha_admin_1", "ocha_admin_2", "ocha_pcode")


## ---------------------------------------------------------------------------
## SECTION 4: columns to drop
## ---------------------------------------------------------------------------

# every shelter column name seen across any round, all dropped before pivoting
shelter_columns <- c(
  "Camp", "Rented houses", "Rentedhouses", "Host Community", "School Building",
  "School building", "Schoolbuilding", "Religious Building", "Religious building",
  "Religiousbuilding", "Unfinished/Abandoned building", "Unfinished/ Abandoned building",
  "Unfinishedbuilding", "Informal/random/irregular settlements or collective shelters",
  "Informal settlements", "Informalsettlements", "Hotel/Motel", "Hotel Motel",
  "Other Collective center", "Unknown shelter type", "Unknownsheltertype",
  "IDP Owned House", "Military Camps", "Other Informal Settlements",
  "Other Formal Settlements", "Other shelter type", "Other shelter", "Other",
  "Hotel/Motel or short-term rental", "Rental (Habitable)", "Rental(Uninhabitable)",
  "Rental (Uninhabitable)", "Own Property", "Non-residential structure",
  "Other formal settlements/ collective centres", "Households Coming From Camp",
  "Apartment/House (not owned) (habitable)", "Apartment/House(not owned)(uninhabitable)",
  "Tent/Caravan/makeshift shelter/mud or block house",
  "Public Buildings or Collective shelters", "Other Critical shelter",
  "IDPs in Camps/ transit camps", "Rented Hotel", "Rented House", "With Relative",
  "With HC - non-Relative", "Mosques/ Holly Shrines",
  "Abandoned/public buildings /under construction", "Collective centres",
  "Unknown or other"
)

# hyperlink columns present in later rounds, also dropped
link_columns <- c("Open Street Map", "Google Map", "Bing Map")


## ---------------------------------------------------------------------------
## SECTION 5: shared header-cleaning helpers
## ---------------------------------------------------------------------------

# strips Arabic text, empty brackets and trailing punctuation from a bilingual column name
clean_bilingual_name <- function(x) {
  x <- str_remove_all(x, "[\u0600-\u06FF\u0750-\u077F\uFB50-\uFDFF\uFE70-\uFEFF]")  # drop Arabic characters
  x <- str_remove_all(x, "\\(\\s*\\)")                                              # drop now-empty brackets
  x <- str_remove(x, "[/\\s]+$")                                                    # drop trailing slash or space
  x <- str_squish(x)                                                                # collapse internal whitespace
  return(x)
}

# applies all three crosswalks to a tibble's column names, in a fixed order
standardise_headers <- function(data) {
  data <- data %>%
    rename_with(str_trim)                                        # strip leading and trailing whitespace
  data <- data %>%
    rename_with(~ str_remove(.x, "\u0640"))                      # remove the Arabic tatweel character
  data <- data %>%
    rename_with(~ str_replace_all(.x, "\\s+", " "))              # collapse newlines and double spaces
  data <- data %>%
    rename_with(~ if_else(.x %in% names(period_lookup),
                          period_lookup[.x], .x))                # standardise period column names
  data <- data %>%
    rename_with(~ if_else(.x %in% names(identifier_lookup),
                          identifier_lookup[.x], .x))            # standardise identifier and count names
  data <- data %>%
    rename_with(~ if_else(.x %in% names(governorate_lookup),
                          governorate_lookup[.x], .x))           # standardise governorate column names
  return(data)
}


## ---------------------------------------------------------------------------
## SECTION 6: the function that reads and reshapes one round (12 onwards)
## ---------------------------------------------------------------------------

process_round <- function(file_path, sheet_name, skip_rows) {
  raw_data <- read_excel(file_path, sheet = sheet_name, skip = skip_rows)
  raw_data <- raw_data %>%
    standardise_headers()                                        # apply the three header crosswalks
  raw_data <- raw_data %>%
    select(-any_of(shelter_columns))                             # drop shelter columns, not needed
  raw_data <- raw_data %>%
    select(-any_of(link_columns))                                # drop map hyperlink columns
  raw_data <- raw_data %>%
    mutate(across(any_of(code_columns), as.character))           # force code fields to text so rounds bind
  raw_data <- raw_data %>%
    mutate(Governorate = recode(Governorate, !!!governorate_lookup))  # harmonise governorate of displacement
  long_data <- raw_data %>%
    pivot_longer(cols = any_of(all_governorates),
                 names_to = "origin_governorate",
                 values_to = "families_from_origin")             # one row per location per origin governorate
  return(long_data)
}


## ---------------------------------------------------------------------------
## SECTION 7: the manifest of files, sheets and header offsets
## ---------------------------------------------------------------------------

round_manifest <- tibble(
  round_number = c(12, 18, 19, 24, 32, 36, 51, 64, 76, 87, 101, 112, 119, 124, 128, 131, 134),
  file_path = c(
    "Round12_Master_List_IDP_2015_January_15_IOM_DTM.xlsx",
    "Round18_Master_List_IDP_2015_April_10_IOM_DTM.xlsx",
    "Round19_Master_List_IDP_2015_April_25_IOM_DTM.xlsx",
    "Round24_Master_List_IDP_2015_July_02_IOM_DTM.xlsx",
    "Round32_Master_List_IDP_2015_November_5_IOM_DTM.xlsx",
    "Round36_Master_List_IDP_2016_January_07_IOM_DTM.xlsx",
    "05_IOM DTM Master_List_IDP_Round 51_20160803.xlsx",
    "IDPs_MasterList_dataset_DTM_IOMFeb 02, 2017_Round64.xlsx",
    "Round76_Master_List_IDP_2017-07-30_IOM_DTM.xlsx",
    "Round87_Master_List_IDP_2017-1-15_IOM_DTM.xlsx",
    "Round101_IDPs_MasterList_dataset_DTM_IOMAug 15, 2018.xlsx",
    "Round112_Master_List_IDP_2019-10-31_IOM_DTM.xlsx",
    "2021112149393_Round119_Master_List_IDP_2020-12-31_IOM_DTM.xlsx",
    "20221124822960_Round124_Master_List_IDP_2021-21-31_IOM_DTM.xlsx",
    "DTM_Iraq_Round128_Master_List_IDP_2022-12-31_IOM_DTM.xlsx",
    "2024129471816_Round131_Master_List_IDP_2023-12-31_IOM_DTM - Individuals.xlsx",
    "20252102822358_Round134_Master_List_IDP_2024-12-31_IOM_DTM - Individuals (1).xlsx"
  ),
  sheet_name = c(
    "DTM Dataset 15 JAN 2015",      # round 12
    "DTM Dataset 10 APRIL 2015",    # round 18
    "MasterList Date 25 Apr 2015",  # round 19
    "DTM DATASET",                  # round 24
    "DTM DATASET",                  # round 32
    "DTM DATASET",                  # round 36
    "DTM DATASET",                  # round 51
    "Sheet",                        # round 64
    "DTM DATASET",                  # round 76
    "DTM DATASET",                  # round 87
    "Sheet",                        # round 101
    "DTM Dataset",                  # round 112
    "DTM Dataset",                  # round 119
    "DTM Dataset",                  # round 124
    "DTM Dataset",                  # round 128
    "DTM Dataset",                  # round 131
    "DTM Dataset"                   # round 134
  ),
  skip_rows = c(1, 1, 3, 3, 3, 3, 3, 0, 3, 3, 0, 2, 2, 2, 2, 2, 2),
  round_date = as.Date(c(
    "2015-01-15", "2015-04-10", "2015-04-25", "2015-07-02", "2015-11-05",
    "2016-01-07", "2016-08-03", "2017-02-02", "2017-07-30", "2018-01-15",
    "2018-08-15", "2019-10-31", "2020-12-31", "2021-12-31", "2022-12-31",
    "2023-12-31", "2024-12-31"
  ))
)

# prepend the subfolder to every filename, run this only once per session
round_manifest <- round_manifest %>%
  mutate(file_path = file.path("IDP_sheets", file_path))  # builds "IDP_sheets/Round12_...xlsx" etc.


## ---------------------------------------------------------------------------
## SECTION 8: run across rounds 12 to 134 and bind the results
## ---------------------------------------------------------------------------

all_results <- round_manifest %>%
  select(file_path, sheet_name, skip_rows) %>%
  pmap(process_round)                                            # returns a list, one tibble per round

all_idp <- map2_dfr(all_results, round_manifest$round_number,
                    ~ mutate(.x, round_number = .y))             # stack all rounds, tagging each


## ---------------------------------------------------------------------------
## SECTION 9: rounds 5 and 8, which use a different 2014 layout
## ---------------------------------------------------------------------------

# reads the three round 5 period sheets and returns place-level period totals in wide form
get_period_from_sheets <- function(file_path, period_sheet_labels) {
  period_data <- map2_dfr(names(period_sheet_labels), period_sheet_labels,
                          function(sheet_name, period_label) {
                            sheet_data <- read_excel(file_path, sheet = sheet_name, skip = 1)
                            sheet_data <- sheet_data %>%
                              rename_with(clean_bilingual_name)                          # strip the Arabic half of each header
                            sheet_data <- sheet_data %>%
                              standardise_headers()                                      # then apply the crosswalks
                            sheet_data <- sheet_data %>%
                              mutate(place_id = as.character(place_id))                  # force to text for joining
                            sheet_data <- sheet_data %>%
                              mutate(families_by_period = rowSums(select(., any_of(all_governorates)),
                                                                  na.rm = TRUE))         # total families across all origins
                            sheet_data <- sheet_data %>%
                              select(place_id, families_by_period)                       # keep only the join key and the total
                            sheet_data <- sheet_data %>%
                              mutate(displacement_period = period_label)                 # tag with the sheet's period
                            return(sheet_data)
                          })
  period_wide <- period_data %>%
    group_by(place_id, displacement_period) %>%
    summarise(families_by_period = sum(families_by_period, na.rm = TRUE),
              .groups = "drop")                                  # one value per place per period
  period_wide <- period_wide %>%
    pivot_wider(names_from = displacement_period,
                values_from = families_by_period)                # period labels become columns
  return(period_wide)
}

# the three round 5 period sheets and the canonical label each carries
period_sheet_labels <- c(
  "3.IDPs by Org- Pre June 2014" = "Pre-June 2014",
  "4. IDPs by Org- Jun & Jul 2014" = "June-July 2014",
  "5.IDPs by Org-August 2014" = "August 2014"
)

# reads one 2014-layout round and returns it long on origin with period columns wide
process_round_2014 <- function(file_path, master_sheet_name, master_skip,
                               period_mode, period_sheet_labels = NULL) {
  raw_data <- read_excel(file_path, sheet = master_sheet_name, skip = master_skip)
  raw_data <- raw_data %>%
    rename_with(clean_bilingual_name)                            # strip the Arabic half of each header
  raw_data <- raw_data %>%
    standardise_headers()                                        # then apply the crosswalks
  raw_data <- raw_data %>%
    mutate(place_id = as.character(place_id))                    # force to text for joining
  raw_data <- raw_data %>%
    mutate(Governorate = recode(Governorate, !!!governorate_lookup))  # harmonise governorate of displacement
  raw_data <- raw_data %>%
    mutate(`Origin Governorate` = recode(`Origin Governorate`,
                                         !!!governorate_lookup)) # harmonise the origin values themselves
  long_origin <- raw_data %>%
    group_by(place_id, Governorate, District, location_name, `Origin Governorate`) %>%
    summarise(families_from_origin = sum(idp_households, na.rm = TRUE),
              .groups = "drop")                                  # collapse to one row per place per origin
  long_origin <- long_origin %>%
    rename(origin_governorate = `Origin Governorate`)            # match the naming used from round 12 on
  if (period_mode == "column") {
    period_wide <- raw_data %>%
      mutate(`Displacement Period` = recode(`Displacement Period`,
                                            !!!period_lookup))   # harmonise the period values
    period_wide <- period_wide %>%
      group_by(place_id, `Displacement Period`) %>%
      summarise(families_by_period = sum(idp_households, na.rm = TRUE),
                .groups = "drop")                                # place-level marginal, one per period
    period_wide <- period_wide %>%
      pivot_wider(names_from = `Displacement Period`,
                  values_from = families_by_period)              # period labels become columns
  } else if (period_mode == "sheets") {
    period_wide <- get_period_from_sheets(file_path, period_sheet_labels)
  }
  combined <- long_origin %>%
    left_join(period_wide, by = "place_id")                      # attach period columns to each origin row
  return(combined)
}

# round 5, where the period breakdown lives in three separate sheets
result_round5 <- process_round_2014(
  file_path = file.path("IDP_sheets", "Round5_Master_List_IDP_2014_August_24_IOM_DTM.xlsx"),
  master_sheet_name = "1.DTM master list 20140826",
  master_skip = 0,
  period_mode = "sheets",
  period_sheet_labels = period_sheet_labels
)

# round 8, where the period is already a column in the master list
result_round8 <- process_round_2014(
  file_path = file.path("IDP_sheets", "Round8_Master_List_IDP_2014_October_26_IOM_DTM.xlsx"),
  master_sheet_name = "1.DTM master list 20141026",
  master_skip = 0,
  period_mode = "column"
)


## ---------------------------------------------------------------------------
## SECTION 10: combine all 19 rounds, attach dates and the unit flag
## ---------------------------------------------------------------------------

result_round5 <- result_round5 %>%
  mutate(round_number = 5)                                       # tag round 5
result_round8 <- result_round8 %>%
  mutate(round_number = 8)                                       # tag round 8

all_idp <- bind_rows(all_idp, result_round5, result_round8)      # one table covering all 19 rounds

# a small table of dates for the two 2014 rounds, to append to the manifest
early_dates <- tibble(
  round_number = c(5, 8),
  round_date = as.Date(c("2014-08-24", "2014-10-26"))
)

# one date lookup covering all 19 rounds
round_dates <- round_manifest %>%
  select(round_number, round_date)
round_dates <- bind_rows(round_dates, early_dates)

all_idp <- all_idp %>%
  left_join(round_dates, by = "round_number")                    # attach the collection date

# from round 119 the disaggregated cells report individuals, not households
all_idp <- all_idp %>%
  mutate(count_unit = if_else(round_number >= 119, "individuals", "households"))

# put the period columns in chronological order at the end of the table
all_idp <- all_idp %>%
  relocate(any_of(all_periods), .after = last_col())


## ---------------------------------------------------------------------------
## SECTION 11: checks and output
## ---------------------------------------------------------------------------

sort(unique(all_idp$round_number))          # expect all 19 round numbers
n_distinct(all_idp$origin_governorate)      # expect 18, not more
sum(is.na(all_idp$round_date))              # expect 0

write_csv(all_idp, "iom_idp_all.csv")

all_idp %>%                                                # start from the combined table
  group_by(round_number) %>%                               # one row per round
  summarise(long_total = sum(families_from_origin, na.rm = TRUE),  # origin sum
            .groups = "drop")                              # ungroup



