do bar for parliament with line over top for ministerial

library(tidyverse)
library(dplyr)
library(scales)

politics_raw <-  read_csv("world_bank_politics_fem_iraq.csv")

dim(politics_raw)
glimpse(politics_raw)

politics_clean <- politics_raw |>
  select(year = Year, name = `Indicator Name`, value = Value) |>
  relocate(year, .after = name) |>
  mutate(name = case_match(
    name, 
    "Proportion of seats held by women in national parliaments (%)" ~ "Seats held by women, national parliament (%)",
    "Proportion of women in ministerial level positions (%)" ~ "Women in ministerial positions (%)"
  ))

politics_clean

ggplot(politics_clean, aes(x = year, y = value, colour = name, shape = name)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8, fill = "white") +
  labs(
    title = "Female political engagement, \nIraq 1997-2025",
    x = "Year",
    y = "Percentage",
    colour = NULL,
    shape = NULL,
    caption = str_wrap(
      paste("Source: World Bank World Development Indicators, 2026.",
      "Note: Parliamentary seats are subject to a 25 per cent constitutional minimum; ministerial appointments do not have legal minimum and are allocated through muhasasa bargaining. Coverage is federal only: Kurdish women in the Council of Representatives are included, but the Kurdistan Parliament (30 per cent quota) and KRG Council of Ministers are not."), 
      width = 110
      )
    ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

ggsave("WB_Iraq_female_political_engagement.png", width = 8, height = 5, dpi = 300)

write_csv(politics_raw, "WB_Iraq_female_political_engagement.csv")
