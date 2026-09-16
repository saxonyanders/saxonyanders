library(tidyverse)
library(scales)
library(dplyr)

icsd_raw <- read_csv("dataset_2026-09-11T08_51_23.193670963Z_DEFAULT_INTEGRATION_IMF.FAD_ICSD_1.0.0.csv")

icsd_clean <- icsd_raw |>
  select(indicator = INDICATOR, matches("^[0-9]{4}$")) |>
  mutate(indicator = case_when(
    indicator == "Gross fixed capital formation, General government, Constant prices, Percent of GDP" ~ "Government",
    indicator == "Gross fixed capital formation, Private sector, Constant prices, Percent of GDP" ~ "Private Sector"
  )) |>
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "pct"
  ) |>
  mutate(year = as.integer(year)) |>
  filter(year >= "2004")

icsd_clean

ggplot(icsd_clean, aes(x = year, y = pct, colour = indicator)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("Government" = "#0072B2", "Private Sector" = "#E69F00")) +
  scale_y_continuous(limits = c(0, 20), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Iraq Gross Fixed Capital Formation",
    subtitle = "General Government vs Private Sector",
    x = NULL, 
    y = "Percentage of GDP",
    color = NULL,
    caption = "Source: IMF.FAD:ICSD(1.0.0), IMF Investment and Capital Stock Database. Accessed September 2026.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

ggsave("IMF_Iraq_capital_formation_gov_priv.png", width = 8, height = 5, dpi = 300)
