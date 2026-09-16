library(tidyverse)
library(dplyr)
library(scales)

ict_raw <- read_csv("world_bank_ict_iraq.csv")

ict_raw <- ict_raw |>
  select(year = Year, name = `Indicator Name`, code = `Indicator Code`, value = Value) |>
  relocate(year, .after = code)


## Internet usage stats ##
internet_usage_raw <- ict_raw |>
  filter(code == "IT.NET.USER.FE.ZS" | code == "IT.NET.USER.MA.ZS" | code == "IT.NET.USER.ZS")

internet_usage_raw <- internet_usage_raw |>
  mutate(code = case_match(                                
    code,                                                   
    "IT.NET.USER.FE.ZS" ~ "Female",                           
    "IT.NET.USER.MA.ZS" ~ "Male",
    "IT.NET.USER.ZS" ~ "Total",
    .default = NA_character_
  )) |>
  filter(year >= "2001")

ggplot(internet_usage_raw, aes(x = year, y = value, colour = code)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Total Internet Usage vs sex aggregation, Iraq 2001-2004",
    x = "Year",
    y = "Percentage usage",
    colour = "Indicator",
    caption = "Source: World Bank World Development Indicators, 2026"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_internet_usage_percent.png", width = 8, height = 5, dpi = 300)
write_csv(internet_usage_raw, "WB_Iraq_internet_usage_percent.csv")


## Mobile, telephone, broadband connection ##

phone_subs_raw <- ict_raw |>
  filter(code == "IT.CEL.SETS" | code == "IT.MLT.MAIN" | code == "IT.NET.BBND")
  
phone_subs <- phone_subs_raw |>
filter(year >= 1970)

ggplot(phone_subs_raw, aes(x = year, y = value, colour = name)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = label_number(                   
    scale = 1e-6,                                             
    suffix = "mn"
  )) +
  labs(
    title = "Total number of phone and broadband subscriptions, \nIraq 1970-2024",
    x = "Year",
    y = "Total number subscriptions",
    colour = "Indicator",
    caption = "Source: World Bank World Development Indicators, 2026. Mn = Million."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_phone_broadband_subs.png", width = 8, height = 5, dpi = 300)
write_csv(internet_usage_raw, "WB_Iraq_phone_broadband_subs.csv")
