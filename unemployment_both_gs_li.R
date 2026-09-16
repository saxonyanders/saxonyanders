library(tidyverse)

print(gs_li_long)

unemployment_both <- gs_li_long |>
  filter(
    GENDER == "Female" | GENDER == "Male",
    INDICATOR == "Unemployment Rate by Age, Modeled ILO Estimate, Rate",
    AGE_GROUP == "15+ yrs"
  )
dim(unemployment_both)
glimpse(unemployment_both)
print(unemployment_both, n = 142)

unemployment_clean <- unemployment_both |>
  select(year, GENDER, value) |> #creates a new data table with just year and value, as all other rows are same eg 'country' = 'iraq' indicator = 'LFP, Modeled ILO Estimate, rate'
  mutate(year = as.numeric(year)) |> # change 'year' column type to number instead of text 'chr'
  filter(year >= 1991)

dim(unemployment_clean)
glimpse(unemployment_clean)
print(unemployment_clean, n = 74)

ggplot(unemployment_clean, aes(x = year, y = value, colour = GENDER)) + # plot line graph on male vs female LFP, with 'gender' column as the line indicator
  geom_line() +
  geom_point() +
  labs(
    title = "Unemployment rate in Iraq, by sex, 1991-2025",
    x = "Year",
    y = "Unemployment rate (% of population aged 15+)",
    colour = "Sex",
    caption = "Source: ILO modelled estimates, via IMF Gender Data Hub."
  ) +
  theme_minimal()

ggsave("outputs/iraq_unemployment_by_sex.png", width = 8, height = 5, dpi = 300)

unemployment_vs_flfp <- gs_li_long |>
  filter(
    GENDER == "Female",
    INDICATOR == "Unemployment Rate by Age, Modeled ILO Estimate, Rate" | INDICATOR == "Labor Force Participation, Modeled ILO Estimate, Rate",
    AGE_GROUP == "15+ yrs"
  )
dim(unemployment_vs_flfp)
glimpse(unemployment_vs_flfp)
print(unemployment_vs_flfp, n = 142)

unemployment_vs_flfp_clean <- unemployment_vs_flfp |>
  select(year, INDICATOR, value) |> #creates a new data table with just year and value, as all other rows are same eg 'country' = 'iraq' indicator = 'LFP, Modeled ILO Estimate, rate'
  mutate(INDICATOR = recode(INDICATOR,
                            "Labor Force Participation, Modeled ILO Estimate, Rate" = "LFP rate",
                            "Unemployment Rate by Age, Modeled ILO Estimate, Rate" = "Unemployment rate")) |>
  mutate(year = as.numeric(year)) |> # change 'year' column type to number instead of text 'chr'
  filter(year >= 1991)

dim(unemployment_vs_flfp_clean)
glimpse(unemployment_vs_flfp_clean)
print(unemployment_vs_flfp_clean, n = 74)

ggplot(unemployment_vs_flfp_clean, aes(x = year, y = value, colour = INDICATOR)) + # plot line graph on male vs female LFP, with 'gender' column as the line indicator
  geom_line() +
  geom_point() +
  labs(
    title = "Female Unemployment Rate vs Labour Force Participation \nin Iraq, 1991-2025",
    x = "Year",
    y = "Rate (% of population aged 15+)",
    colour = "Indicator",
    caption = "Source: ILO modelled estimates, via IMF Gender Data Hub."
  ) +
  theme(legend.position = "bottom")

ggsave("outputs/iraq_female_unemployment_vs_flfp.png", width = 10, height = 6, dpi = 300)
