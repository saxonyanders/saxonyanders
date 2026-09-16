getwd()
library(tidyverse)

print(gs_li_long)

lfp_both <- gs_li_long |>
  filter(
    GENDER == "Female" | GENDER == "Male",
    INDICATOR == "Labor Force Participation, Modeled ILO Estimate, Rate",
    AGE_GROUP == "15+ yrs"
  )
dim(lfp_both)
glimpse(lfp_both)
print(lfp_both, n = 142)

lfp_clean <- lfp_both |>
  select(year, GENDER, value) |> #creates a new data table with just year and value, as all other rows are same eg 'country' = 'iraq' indicator = 'LFP, Modeled ILO Estimate, rate'
  mutate(year = as.numeric(year)) |> # change 'year' column type to number instead of text 'chr'
  filter(year >= 1990)

dim(lfp_clean)
glimpse(lfp_clean)
print(lfp_clean, n = 76)

ggplot(lfp_clean, aes(x = year, y = value, colour = GENDER)) + # plot line graph on male vs female LFP, with 'gender' column as the line indicator
  geom_line() +
  geom_point()

ggplot(lfp_clean, aes(x = year, y = value, colour = GENDER)) + # plot line graph on male vs female LFP, with 'gender' column as the line indicator
  geom_line() +
  geom_point() +
  labs(
    title = "Labour Force Participation rate in Iraq, by sex, 1990-2027",
    x = "Year",
    y = "Participation rate (% of population aged 15+)",
    colour = "Sex",
    caption = "Source: ILO modelled estimates, via IMF Gender Data Hub. 2025-2027 are projections."
  ) +
  theme_minimal()

ggsave("outputs/iraq_lfp_by_sex.png", width = 8, height = 5, dpi = 300)
