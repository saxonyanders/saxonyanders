library(tidyverse)
library(dplyr)

demo_raw <- read_csv("world_bank_demography_iraq.csv")

unique(demo_raw$`Indicator Name`)

## FERTILITY ANALYSIS ##

# Filter
fertility_all <- demo_raw |>
  filter(`Indicator Name` %in% c(
    "Adolescent fertility rate (births per 1,000 women ages 15-19)",
    "Contraceptive prevalence, any method (% of married women ages 15-49)",
    "Fertility rate, total (births per woman)",
    "Women who were first married by age 15 (% of women ages 20-24)",      
    "Women who were first married by age 18 (% of women ages 20-24)",
    "Total fertility rate (TFR) (births per woman): Q1 (lowest)",
    "Age at first marriage, female",
    "Age at first marriage, male"
  ))


# Marriage
marriage_age <- fertility_all |>
  filter(`Indicator Name` %in% c(
    "Age at first marriage, female",
    "Age at first marriage, male")) |>
  mutate(
    `Indicator Name` = recode(`Indicator Name`,
    "Age at first marriage, female" = "Female",
    "Age at first marriage, male" = "Male"))

ggplot(marriage_age, aes(x = Year, y = Value, colour = `Indicator Name`)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Age at first marriage by sex \nIraq 1977-2012",
    x = "Year",
    y = "Age",
    colour = "Sex",
    caption = "Source: World Bank World Development Indicators, 2026") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_age_first_marriage_sex.png", width = 8, height = 5, dpi = 300)


# Fertility rate
fertility_total <- fertility_all |>
  filter(`Indicator Name` %in% c(
    "Fertility rate, total (births per woman)",
    "Total fertility rate (TFR) (births per woman): Q1 (lowest)")) |>
  mutate(
    `Indicator Name` = recode(`Indicator Name`,
                              "Fertility rate, total (births per woman)" = "Total fertility rate (TFR)",
                              "Total fertility rate (TFR) (births per woman): Q1 (lowest)" = "TFR by first quintile (poorest 20%)"))

ggplot(fertility_total, aes(x = Year, y = Value, colour = `Indicator Name`)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Total fertility rate vs TFR first quintile \nIraq 1960-2024",
    x = "Year",
    y = "Births per woman",
    caption = "Source: World Bank World Development Indicators, 2026") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_fertility_rate.png", width = 8, height = 5, dpi = 300)

write_csv(fertility_total, "WB_Iraq_fertility_total.csv")


# Adolescent births
adolescent_fertility <- fertility_all |>
  filter(`Indicator Name` %in% c(
    "Adolescent fertility rate (births per 1,000 women ages 15-19)"))

ggplot(adolescent_fertility, aes(x = Year, y = Value)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Adolescent fertility rate Iraq \n(births per 1,000 women ages 15-19, 1960-2024)",
    x = "Year",
    y = "Births per 1,000 women",
    caption = "Source: World Bank World Development Indicators, 2026") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_adolescent_fertility_rate.png", width = 8, height = 5, dpi = 300)


## Adolescent births vs TFR ##

adolescent_fertility
fertil_TFR

adolescent_fertility <- adolescent_fertility |>
  mutate(
    pw = Value / 1000
  )

adolescent_fertility <- adolescent_fertility |>
  select(!Value) |>
  select(Year, `Indicator Name`, `Indicator Code`, Value = pw)

fertil_TFR <- fertility_total |>
  filter(`Indicator Code` == "SP.DYN.TFRT.IN")

adol_tfr <- bind_rows(adolescent_fertility, fertil_TFR)

adol_tfr <- adol_tfr |>                                                   
  mutate(`Indicator Name` = case_match(                                    
    `Indicator Name`,                                                      
    "Adolescent fertility rate (births per 1,000 women ages 15-19)" ~ "AFR",
    "Total fertility rate (TFR)" ~ "TFR"   
  ))

adol_tfr

ggplot(adol_tfr, aes(x = Year, y = Value, colour = `Indicator Name`)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = NULL,
    x = "Year",
    y = "Births per woman",
    colour = NULL,
    shape = NULL,
    caption = "Source: World Bank World Development Indicators, 2026.") +
  facet_wrap(vars(`Indicator Name`),
             scales = "free_y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

ggsave("WB_Iraq_afr_vs_tfr.png", width = 8, height = 5, dpi = 300)


## POPULATION URBAN / RURAL ##

# Filtering
urban_rural <- demo_raw |>
  filter(`Indicator Name` %in% c(
    "Rural population (% of total population)",
    "Rural population, female (% of total)",                          
    "Rural population, male (% of total)",                              
    "Urban population, female (% of total)",                               
    "Urban population, male (% of total)")) 

# Rural population
rural_pop <- urban_rural |>
  filter(`Indicator Name` %in% c(
    "Rural population (% of total population)"))

ggplot(rural_pop, aes(x = Year, y = Value)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Rural Population % Iraq 1960-2025",
    x = "Year",
    y = "Percentage rural",
    caption = "Source: World Bank World Development Indicators, 2026") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_rural_pop.png", width = 8, height = 5, dpi = 300)


# Rural vs urban population
rural_urb_sex <- urban_rural |>
  filter(`Indicator Name` %in% c(
    "Rural population, female (% of total)",                          
    "Rural population, male (% of total)",                              
    "Urban population, female (% of total)",                               
    "Urban population, male (% of total)")) |>
  mutate(
    `Indicator Name` = recode(`Indicator Name`,
                              "Rural population, female (% of total)" = "Female rural",                          
                              "Rural population, male (% of total)" = "Male rural",                              
                              "Urban population, female (% of total)" = "Female urban",                               
                              "Urban population, male (% of total)" = "Male urban"))

ggplot(rural_urb_sex, aes(x = Year, y = Value, colour = `Indicator Name`)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(
    title = "Total Iraq population by location and sex, 1980-2015",
    x = "Year",
    y = "Percent of total population",
    colour = "Location / Sex",
    caption = "Source: World Bank World Development Indicators, 2026") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_rural_urban_pop_sex.png", width = 8, height = 5, dpi = 300)


## Population pyramids ## 

# Filtering
pop_age_sex <- demo_raw |>
  filter(`Indicator Name` %in% c(
    "Population ages 0-14, female (% of female population)",               
    "Population ages 0-14, male (% of male population)",                   
    "Population ages 15-19, female (% of female population)",              
    "Population ages 15-19, male (% of male population)",                  
    "Population ages 20-24, female (% of female population)",              
    "Population ages 20-24, male (% of male population)",                  
    "Population ages 25-29, female (% of female population)",              
    "Population ages 25-29, male (% of male population)",                  
    "Population ages 30-34, female (% of female population)",              
    "Population ages 30-34, male (% of male population)",                  
    "Population ages 35-39, female (% of female population)",              
    "Population ages 35-39, male (% of male population)",                  
    "Population ages 40-44, female (% of female population)",              
    "Population ages 40-44, male (% of male population)",                  
    "Population ages 45-49, female (% of female population)",              
    "Population ages 45-49, male (% of male population)",                  
    "Population ages 50-54, female (% of female population)",              
    "Population ages 50-54, male (% of male population)",                  
    "Population ages 55-59, female (% of female population)",              
    "Population ages 55-59, male (% of male population)",                  
    "Population ages 60-64, female (% of female population)",              
    "Population ages 60-64, male (% of male population)",                  
    "Population ages 65 and above, female (% of female population)",       
    "Population ages 65 and above, male (% of male population)"))

# Male pop.
pop_age_m <- pop_age_sex |>
  filter(`Indicator Name` %in% c(
    "Population ages 0-14, male (% of male population)",                   
    "Population ages 15-19, male (% of male population)",                  
    "Population ages 20-24, male (% of male population)",                  
    "Population ages 25-29, male (% of male population)",                  
    "Population ages 30-34, male (% of male population)",                  
    "Population ages 35-39, male (% of male population)",                  
    "Population ages 40-44, male (% of male population)",                  
    "Population ages 45-49, male (% of male population)",                  
    "Population ages 50-54, male (% of male population)",                  
    "Population ages 55-59, male (% of male population)",                  
    "Population ages 60-64, male (% of male population)",                  
    "Population ages 65 and above, male (% of male population)")) |>
  filter(Year %in% c(
    "1960", "1970", "1980",
    "1990", "2000", "2010", 
    "2020", "2025")) |>
  mutate(
    Sex = "Male",
    Age = recode(`Indicator Name`,
                              "Population ages 0-14, male (% of male population)" = "0-14",               
                              "Population ages 15-19, male (% of male population)" = "15-19",             
                              "Population ages 20-24, male (% of male population)" = "20-24",
                              "Population ages 25-29, male (% of male population)" = "25-29",
                              "Population ages 30-34, male (% of male population)" = "30-34",               
                              "Population ages 35-39, male (% of male population)" = "35-39",                  
                              "Population ages 40-44, male (% of male population)" = "40-44",                 
                              "Population ages 45-49, male (% of male population)" = "45-49",                  
                              "Population ages 50-54, male (% of male population)" = "50-54",                  
                              "Population ages 55-59, male (% of male population)" = "55-59",                  
                              "Population ages 60-64, male (% of male population)" = "60-64",                  
                              "Population ages 65 and above, male (% of male population)" = "65+"
    ))


# Female pop.
pop_age_f <- pop_age_sex |>
  filter(`Indicator Name` %in% c(
    "Population ages 0-14, female (% of female population)",               
    "Population ages 15-19, female (% of female population)",              
    "Population ages 20-24, female (% of female population)",              
    "Population ages 25-29, female (% of female population)",              
    "Population ages 30-34, female (% of female population)",              
    "Population ages 35-39, female (% of female population)",              
    "Population ages 40-44, female (% of female population)",              
    "Population ages 45-49, female (% of female population)",              
    "Population ages 50-54, female (% of female population)",              
    "Population ages 55-59, female (% of female population)",              
    "Population ages 60-64, female (% of female population)",              
    "Population ages 65 and above, female (% of female population)")) |>
  filter(Year %in% c(
    "1960", "1970", "1980",
    "1990", "2000", "2010", 
    "2020", "2025")) |>
  mutate(
    Sex = "Female",
    Age = recode(`Indicator Name`,
                              "Population ages 0-14, female (% of female population)" = "0-14",               
                              "Population ages 15-19, female (% of female population)" = "15-19",             
                              "Population ages 20-24, female (% of female population)" = "20-24",
                              "Population ages 25-29, female (% of female population)" = "25-29",
                              "Population ages 30-34, female (% of female population)" = "30-34",               
                              "Population ages 35-39, female (% of female population)" = "35-39",                  
                              "Population ages 40-44, female (% of female population)" = "40-44",                 
                              "Population ages 45-49, female (% of female population)" = "45-49",                  
                              "Population ages 50-54, female (% of female population)" = "50-54",                  
                              "Population ages 55-59, female (% of female population)" = "55-59",                  
                              "Population ages 60-64, female (% of female population)" = "60-64",                  
                              "Population ages 65 and above, female (% of female population)" = "65+"
    ))


# Bind together
pop_pyramid <- bind_rows(pop_age_m, pop_age_f) |>
  mutate(
    Age = factor(Age, levels = c("0-14", "15-19", "20-24", "25-29", "30-34",
                                 "35-39", "40-44", "45-49", "50-54", "55-59",
                                 "60-64", "65+")),
    Value_signed = if_else(Sex == "Male", -Value, Value)
  )


# Plot
ggplot(pop_pyramid, aes(x = Age, y = Value_signed, fill = Sex)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(values = c("Female" = "#D55E00",
                               "Male" = "#0072B2")) +
  facet_wrap(~ Year) +
  labs(x = NULL,
       y = "% of population",
       fill = NULL,
       title = "Iraq population pyramids, 1960-2025") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_population_pyramids_1960_2025_facets.png", width = 11, height = 9, dpi = 300)

write_csv(pop_pyramid, "WB_Iraq_pop_pyramid.csv")

# Plot layering 1970 and 2025
pop_pyramid_compare <- pop_pyramid |>
  filter(Year %in% c("1970", "2025"))

ggplot(pop_pyramid_compare, aes(x = Age, y = Value_signed, fill = Sex)) +
  geom_col(
    data = filter(pop_pyramid_compare, Year == "1970"),
    aes(alpha = "1970"),
    width = 0.7,
    position = "identity"
  ) +
  geom_col(
    data = filter(pop_pyramid_compare, Year == "2025"),
    aes(alpha = "2025"),
    width = 0.4,                     
    position = "identity"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(values = c("Female" = "#D55E00",
                               "Male" = "#0072B2")) +
  scale_alpha_manual(values = c("1970" = 0.35, "2025" = 0.9),
                     name = "Year") +
  labs(x = NULL,
       y = "Percentage of sex population",
       title = NULL,
       caption = "Source: World Bank Development Indicators 2026, SP.POP") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_population_pyramid_1970_vs_2025_overlay.png", width = 8, height = 7, dpi = 300)

