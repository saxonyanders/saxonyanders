library(tidyverse)

nifl_table <- read_csv("data/EMP_NIFL_SEX_ECO_GEO_RT_A-filtered-2026-06-28.csv") |>
  filter(ref_area.label == "Iraq", sex.label == "Female") |>
  filter(classif1.label %in% c(
    "Economic activity (Broad sector): Total",
    "Economic activity (Broad sector): Agriculture",
    "Economic activity (Broad sector): Industry",
    "Economic activity (Broad sector): Services")) |>
  mutate(sector = str_remove(classif1.label, "Economic activity \\(Broad sector\\): "),
         area   = str_remove(classif2.label, "Area type: ")) |>
  select(sector, area, informal_pct = obs_value) |>
  mutate(informal_pct = round(informal_pct, 1)) |>
  pivot_wider(names_from = area, values_from = informal_pct)

print(nifl_table)
write_csv(nifl_table, "outputs/iraq_women_informality_2021.csv")

nifl_table_male <- read_csv("data/EMP_NIFL_SEX_ECO_GEO_RT_A-filtered-2026-06-28.csv") |>
  filter(ref_area.label == "Iraq", sex.label == "Male") |>
  filter(classif1.label %in% c(
    "Economic activity (Broad sector): Total",
    "Economic activity (Broad sector): Agriculture",
    "Economic activity (Broad sector): Industry",
    "Economic activity (Broad sector): Services")) |>
  mutate(sector = str_remove(classif1.label, "Economic activity \\(Broad sector\\): "),
         area   = str_remove(classif2.label, "Area type: ")) |>
  select(sector, area, informal_pct = obs_value) |>
  mutate(informal_pct = round(informal_pct, 1)) |>
  pivot_wider(names_from = area, values_from = informal_pct)

print(nifl_table_male)
write_csv(nifl_table_male, "outputs/iraq_men_informality_2021.csv")
