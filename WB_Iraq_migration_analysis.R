library(tidyverse)
library(dplyr)
library(scales)


## IDP 2011-2023 ##
idp <- read_csv("world_bank_idp_iraq.csv")

ggplot(idp, aes(x = Year, y = Value)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = label_number(                   
    scale = 1e-6,                                             
    suffix = "mn"
  )) +
  labs(
    title = "Total number of IDP, \nIraq 2011-2023",
    x = "Year",
    y = "Total number IDP",
    caption = "Source: World Bank World Development Indicators, 2026. Mn = Million."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_IDP_2011_2023.png", width = 8, height = 5, dpi = 300)

write_csv(idp, "WB_Iraq_IDP_2011_2023.csv")


# Net migration 1960-2025 ##

net_migrate <- read_csv("world_bank_migrate_iraq.csv")

ggplot(net_migrate, aes(x = Year, y = Value)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = label_number(                   
    scale = 1e-3
  )) +
  labs(
    title = "Net migration, \nIraq 1960-2025",
    x = "Year",
    y = "Total migration (1000s)",
    caption = "Source: World Bank World Development Indicators, 2026."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_migrate_1960_2025.png", width = 8, height = 5, dpi = 300)

write_csv(net_migrate, "WB_Iraq_migrate_1960_2025.csv")

## Join IDP and net migration ##

unique(idp$`Indicator Name`)

idp <- idp |>
  mutate(`Indicator Name` = case_match(                                
    `Indicator Name`,                                                   
    "Internally displaced persons, new displacement associated with conflict and violence (number of cases)" ~ "New IDP"))   

idp <- idp |>
  select(year = Year, value = Value, indicator = `Indicator Name`)

idp <- idp |>
  relocate(year, .after = indicator)

net_migrate <- net_migrate |>
  select(year = Year, value = Value, indicator = `Indicator Name`) |>
  relocate(year, .after = indicator)

net_plus_idp <- bind_rows(net_migrate, idp) |>
  filter(year >=1980)

ggplot(net_plus_idp, aes(x = year, y = value, colour = indicator)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = label_number(                   
    scale = 1e-3
  )) +
  labs(
    title = "Net migration vs IDP migration, \nIraq 1960-2025 / 2011-2023",
    x = "Year",
    y = "Migration (1000s)",
    colour = "Indicator",
    caption = "Source: World Bank World Development Indicators, 2026. \nNet migration is national in- and out-country migration. \nNew IDP is new IDP migration due to conflict and/or violence."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("WB_Iraq_migrate_vs_IDP.png", width = 8, height = 5, dpi = 300)

write_csv(net_plus_idp, "WB_Iraq_WB_Iraq_migrate_vs_IDP.csv")