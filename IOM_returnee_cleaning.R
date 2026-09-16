
library(tidyverse)  
library(readxl) 


# maps every governorate spelling variant found across rounds to one canonical name
governorate_lookup <- c(
  "Anbar" = "Anbar",  
  "Al Anbar" = "Anbar",                 
  "Babylon" = "Babylon",                
  "Babil" = "Babylon",             
  "Baghdad" = "Baghdad",                  
  "Basrah" = "Basrah",                
  "Al Basrah" = "Basrah",                 
  "Dahuk" = "Dahuk",                
  "Duhok" = "Dahuk",                      
  "Diyala" = "Diyala",                   
  "Erbil" = "Erbil",                      
  "Kerbala" = "Kerbala",                  
  "Kirkuk" = "Kirkuk",                    
  "Missan" = "Missan",                   
  "Muthanna" = "Muthanna",               
  "Al Muthanna" = "Muthanna",           
  "Najaf" = "Najaf",                      
  "Al Najaf" = "Najaf",                   
  "Ninewa" = "Ninewa",                    
  "Qadissiya" = "Qadissiya",              
  "Al Diwaniya" = "Qadissiya",            
  "Salah al-Din" = "Salah al-Din",        
  "Salah Al-Din" = "Salah al-Din",       
  "Salahal Din" = "Salah al-Din",         
  "Sulaymaniyah" = "Sulaymaniyah",        
  "Al Sulaymaniyah" = "Sulaymaniyah",     
  "Thi-Qar" = "Thi-Qar",                  
  "Thi Qar" = "Thi-Qar",                 
  "Wassit" = "Wassit"                 
)

# the 18 canonical governorate names, used as the pivot target
all_governorates <- unique(unname(governorate_lookup))


# maps every period column heading variant found across rounds to one canonical label
period_lookup <- c(
  "Pre-June 14" = "Pre-June 2014",                                  
  "Pre-June14 Wave" = "Pre-June 2014",                              
  "Pre-June14 Period" = "Pre-June 2014",                            
  "Pre June14 Period of displacement" = "Pre-June 2014",           
  "June-July 14" = "June-July 2014",                               
  "June-July14 Wave" = "June-July 2014",                            
  "June-July14 Period" = "June-July 2014",                       
  "June July14 Period of displacement" = "June-July 2014",        
  "August 14" = "August 2014",                                     
  "August14 Wave" = "August 2014",                                  
  "August14 Period" = "August 2014",                                
  "August 14 Period of displacement" = "August 2014",               
  "Post September 14" = "Post September 2014",                      
  "Post September14 Wave" = "Post September 2014",               
  "Post September14 Period" = "Post September 2014",                
  "Post September14 Period of displacement" = "Post September 2014",
  "Post April15 Wave" = "Post April 2015",                          
  "Post April15 Period" = "Post April 2015",                        
  "Post April15 Period of displacement" = "Post April 2015",        
  "Post Mar16 Period" = "Post March 2016",                          
  "Post March 16 Period of displacement" = "Post March 2016",      
  "Post 17 October 16 Period" = "Post October 2016",                
  "Post 17 October 16 Period of displacement" = "Post October 2016",
  "July 17" = "July 2017",                                          
  "July 17 Period of displacement" = "July 2017",                   
  "Jan 19" = "Jan 2019",                                            
  "Jan 2019" = "Jan 2019",                                          
  "Jan 20" = "Jan 2020",                                            
  "Jan 2020" = "Jan 2020",                                          
  "Jan 2021" = "Jan 2021",                                          
  "Jan 2022" = "Jan 2022",                                        
  "Jan 2023" = "Jan 2023",                                          
  "Jan 2024" = "Jan 2024"                                          
)

# the 14 canonical period labels, kept for reference and later pivoting
all_periods <- unique(unname(period_lookup))


# maps identifier and count column variants to one consistent set of names
identifier_lookup <- c(
  "Place id" = "place_id",                          
  "Place ID" = "place_id",                          
  "Location Name" = "location_name",                
  "Location name in English" = "location_name",    
  "Location_name" = "location_name",                
  "Arabic Name" = "location_name_arabic",          
  "Location name in Arabic" = "location_name_arabic", 
  "Arabic_name" = "location_name_arabic",           
  "IDPs Families" = "returnee_households",          
  "Families" = "returnee_households",               
  "Households" = "returnee_households",             
  "IDPs Individuals" = "returnee_individuals",      
  "Individuals" = "returnee_individuals"            
)

# every shelter column name seen across rounds, all dropped before pivoting
shelter_columns <- c(
  "Private home", "Camp", "Rented houses", "Host Community", "School Building",
  "Religious Building", "Unfinished/Abandoned building", "Unfinished/ Abandoned building",
  "Other Informal Settlements", "Hotel/Motel", "Other Formal Settlements",
  "Unknown shelter type", "Habitual residence", "Host Families", "Informal settlements",
  "Other shelter type", "Habitual_residence", "Host_families", "Hotel_Motel",
  "Informal_settlements", "Other", "Religious_building", "Rented_houses",
  "School_building", "Unfinished_Abandoned_building", "Unknown_shelter_type",
  "Habitual residnece (Habitable)", "Habitual residence (Uninhabitable)",
  "Households returned From Camp", "Non-residential structure",
  "Other formal settlements/ collective centres", "Other shelters",
  "Residence of Origin (Habitable)", "Residence of Origin (Uninhabitable)",
  "Tent/Caravan/makeshift shelter/mud or block house",
  "Public Buildings or Collective shelters", "Other Critical shelter"
)

# code and identifier fields that must be text, since leading zeros vary by round
code_columns <- c("Place Code", "District Code", "OCHA Adm1 PCode", "OCHA ADM2 PCode",
                  "OCHA_PCode", "OCHA Admin 1", "OCHA Admin 2", "OCHA PCode",
                  "OCHA admin 1", "OCHA admin 2", "Location ID")

# hyperlink columns present in later rounds, also dropped
link_columns <- c("Open Street Map", "Google Map", "Bing Map")


process_returnee_round <- function(file_path, sheet_name, skip_rows) {
  raw_data <- read_excel(file_path, sheet = sheet_name, skip = skip_rows)
  raw_data <- raw_data %>%
    rename_with(~ str_replace_all(.x, "\\s+", " "))            # collapse newlines and double spaces in headers
  raw_data <- raw_data %>%
    rename_with(str_trim)                                       # strip leading and trailing whitespace
  raw_data <- raw_data %>%
    select(-any_of(shelter_columns))                            # drop shelter columns, not needed
  raw_data <- raw_data %>%
    select(-any_of(link_columns))                               # drop map hyperlink columns
  raw_data <- raw_data %>%
    mutate(across(any_of(c("Latitude", "Longitude")), as.numeric))  # force coordinates to numeric
   raw_data <- raw_data %>%
    mutate(across(any_of(code_columns), as.character))   # force all code fields to text so rounds bind
  raw_data <- raw_data %>%
    rename_with(~ if_else(.x %in% names(identifier_lookup),
                          identifier_lookup[.x], .x))           # standardise id and count column names
  raw_data <- raw_data %>%
    rename_with(~ if_else(.x %in% names(period_lookup),
                          period_lookup[.x], .x))               # standardise period column names
  raw_data <- raw_data %>%
    rename_with(~ if_else(.x %in% names(governorate_lookup),
                          governorate_lookup[.x], .x))          # standardise governorate column names
  raw_data <- raw_data %>%
    mutate(place_id = as.character(place_id))                   # force to character so rounds bind cleanly
  raw_data <- raw_data %>%
    mutate(Governorate = recode(Governorate, !!!governorate_lookup)) # harmonise governorate of return values
  long_data <- raw_data %>%
    pivot_longer(cols = any_of(all_governorates),
                 names_to = "last_displacement_governorate",
                 values_to = "returnees_from_last_displacement") # one row per location per source governorate
  return(long_data)
}

round_manifest <- tibble(
  round_number = c(18, 19, 24, 32, 36, 51, 64, 76, 87, 101, 112, 119, 124, 128, 131, 134),
  file_path = c(
    "Round18_Master_List_Returnee_2015_April_10_IOM_DTM.xlsx",
    "Round19_Master_List_Returnee_2015_April_25_IOM_DTM.xlsx",
    "Round24_Master_List_Returnee_2015_July_02_IOM_DTM.xls",
    "Round32_Master_List_Returnee_2015_November_5_IOM_DTM.xlsx",
    "Round36_Master_List_Returnee_2016_January_07_IOM_DTM.xlsx",
    "Round51_Master_List_Returnee_2016_08_03_IOM_DTM.xlsx",
    "Round64_Master_List_Returnee_2017_2_2_IOM_DTM.xlsx",
    "Round76_Master_List_Returnee_2017_07_30_IOM_DTM.xlsx",
    "Round87_Master_List_Returnee_2017_1_15_IOM_DTM.xlsx",
    "Round101_Returnee_MasterList_dataset_DTM_IOMAug 15, 2018.xlsx",
    "Round112_Master_List_Returnee_2019_10_31_IOM_DTM.xlsx",
    "20211121443860_Round119_Master_List_Returnee_2020_12_31_IOM_DTM.xlsx",
    "20221124856755_Round124_Master_List_Returnee_2021_12_31_IOM_DTM.xlsx",
    "DTM_Iraq_Round128_Master_List_Returnees_IOM_DTM.xlsx",
    "2024129486987_Round131_Master_List_Returnee_2023_12_31_IOM_DTM - Individuals_0.xlsx",
    "20252102917934_Round134_Master_List_Returnee_2024_12_31_IOM_DTM - Individuals (1).xlsx"
  ),
  sheet_name = c(
    "Returnee Dataset 10 APRIL 2015",  
    "Returnee Dataset 25 APRIL 2015",   
    "RETURNEE DATASET",                
    "RETURNEE DATASET",              
    "RETURNEE DATASET",                 
    "RETURNEE DATASET",                 
    "RETURNEE DATASET",              
    "RETURNEE DATASET",                
    "RETURNEE DATASET",               
    "Sheet",                          
    "RETURNEE DATASET",                
    "RETURNEE DATASET",              
    "RETURNEE DATASET",             
    "RETURNEE DATASET",                
    "RETURNEE DATASET",               
    "RETURNEE DATASET"                  
  ),
  skip_rows = c(0, 2, 3, 3, 3, 3, 3, 3, 3, 0, 2, 2, 2, 2, 2, 2),
  round_date = as.Date(c(
    "2015-04-10", "2015-04-25", "2015-07-02", "2015-11-05", "2016-01-07",
    "2016-08-03", "2017-02-02", "2017-07-30", "2018-01-15", "2018-08-15",
    "2019-10-31", "2020-12-31", "2021-12-31", "2022-12-31", "2023-12-31",
    "2024-12-31"
  ))
)

# prepend the subfolder to every filename in one step
round_manifest <- round_manifest %>%
  mutate(file_path = file.path("returnee_sheets", file_path))  # builds "returnee_sheets/Round18_...xlsx" etc.

all_results <- round_manifest %>%
  select(file_path, sheet_name, skip_rows) %>%
  pmap(process_returnee_round)                                  # returns a list, one tibble per round

all_returnee <- map2_dfr(all_results, round_manifest$round_number,
                         ~ mutate(.x, round_number = .y))       # stack all rounds, tagging each with its number

all_returnee <- all_returnee %>%
  left_join(round_manifest %>% select(round_number, round_date),
            by = "round_number")                                # attach the collection date for each round



# from round 119 the disaggregated cells report individuals, not households
all_returnee <- all_returnee %>%
  mutate(count_unit = if_else(round_number >= 119, "individuals", "households"))

write_csv(all_returnee, "iom_returnee_all.csv")                 # single combined output file
