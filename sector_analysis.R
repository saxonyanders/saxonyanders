library(tidyverse)

emp_raw <- read_csv("data/ILO-EMP_TEMP_SEX_ECO_NB_time_series.csv")

emp_long <- emp_raw |>
  pivot_longer(
    cols = -period, #fold every column except period
    names_to = "series",
    values_to = "value"
  )

glimpse(emp_long)

emp_coded <- emp_long |> #this section is written in regex and means "find and extract by pattern"
  mutate(
    eco_code = str_extract(series, "ECO_[A-Z0-9_]+(?=\\.SEX)"),
    sex_code = str_extract(series, "SEX_[FMT]")
  )

emp_coded |> 
  select(series, eco_code, sex_code) |> 
           print(n = 30)

emp_tidy <- emp_coded |>
  mutate(
    level = case_when( # multi-code 'if'
      str_starts(eco_code, "ECO_SECTOR")  ~ "Broad",
      str_starts(eco_code, "ECO_AGGREGATE")  ~ "Aggregate",
      str_starts(eco_code, "ECO_ISIC4") ~ "ISIC4",
      str_starts(eco_code, "ECO_AGNAG") ~ "AgNonAg"
    ),
    sex = recode(sex_code,
      "SEX_F" = "Female", "SEX_M" = "Male", "SEX_T" = "Total")
  )

glimpse(emp_tidy)
emp_tidy |> count(level, sex)

emp_tidy |>
  filter(level == "Broad") |>
  distinct(eco_code)

broad_dist <- emp_tidy |> #this generates new data table of just the 3 broad sectors
  filter(level == "Broad",
         eco_code %in% c("ECO_SECTOR_AGR",
                         "ECO_SECTOR_IND",
                         "ECO_SECTOR_SER")) |>
  group_by(sex) |>
  mutate(share = value / sum(value) * 100) |>
  ungroup()

broad_dist |>
  select(sex, eco_code, value, share) |> #what data to be displayed
  arrange(sex, eco_code)

#create table, build a wide table: one row per sector, columns for women's and men's counts AND shares.

sector_table <- broad_dist |>
  filter(sex %in% c("Female", "Male")) |> #"c" is combine / concatenate "%in%" is 'is this value somewhere on the right'
  select(sex, eco_code, value, share) |>
  mutate(
    sector = recode(eco_code,
      "ECO_SECTOR_AGR" = "Agriculture",
      "ECO_SECTOR_IND" = "Industry",
      "ECO_SECTOR_SER" = "Services"),
    value = round(value, 0), #thousands of persons, whole numbers
    share = round(share, 1) #one decimal place
  ) |>
  select(sector, sex, value, share) |>
  pivot_wider(
    names_from = sex,
    values_from = c(value, share)
  )

print(sector_table)

sector_table_clean <- sector_table |>
  rename(
    `Women (000s)` = value_Female,
    `Men (000s)` = value_Male,
    `Women (%)` = share_Female,
    `Men (%)` = share_Male
  )
write_csv(sector_table_clean, "outputs/iraq_sector_distribution_2021.csv")

# plot broad sector data

broad_dist |>
  filter(sex %in% c("Female", "Male")) |>
  mutate(sector = recode(eco_code,
                         "ECO_SECTOR_AGR" = "Agriculture",
                         "ECO_SECTOR_IND" = "Industry",
                         "ECO_SECTOR_SER" = "Services")) |>
  ggplot(aes(x = sector, y = value, fill = sex)) +
  geom_col(position = "dodge") + # dodge is for side by side comparison
  labs(
    title = "Employment by sector and sex, Iraq, 2021",
    x = NULL,
    y = "Employment (thousands)",
    fill = "Sex",
    caption = "Source: ILO, ILO-EMP_TEMP_SEX_ECO_NB_time_series, \n based on Iraq Labour Force Survey 2021."
  ) +
  theme_minimal()

ggsave("outputs/iraq_broad_sector_sex_2021.png", width = 10, height = 6, dpi = 300, bg = "white")


# create ISIC4 outputs

# Women's (and men's) total employment — the ISIC Total row, used as denominator
isic_totals <- emp_tidy |>
  filter(level == "ISIC4", eco_code == "ECO_ISIC4_TOTAL") |>
  select(sex, total = value)

# All individual ISIC categories (exclude the Total roll-up and the
# "Not elsewhere classified" residual so it's clean to filter later)
ISIC4_dist <- emp_tidy |>
  filter(level == "ISIC4",
         eco_code != "ECO_ISIC4_TOTAL",
         eco_code != "ECO_ISIC4_X") |>
  left_join(isic_totals, by = "sex") |>
  mutate(share = value / total * 100)

ISIC4_dist |>
  filter(sex == "Female") |>
  select(eco_code, value, share) |>
  arrange(desc(share)) |>
  print(n = 30)

ISIC4_dist |>
  filter(sex == "Female") |>
  summarise(total_share = sum(share, na.rm = TRUE))


ISIC4_named <- ISIC4_dist |>
  mutate(activity = recode(eco_code,
                           "ECO_ISIC4_A" = "Agriculture, forestry & fishing",
                           "ECO_ISIC4_B" = "Mining & quarrying",
                           "ECO_ISIC4_C" = "Manufacturing",
                           "ECO_ISIC4_D" = "Electricity & gas supply",
                           "ECO_ISIC4_E" = "Water, sewerage & waste",
                           "ECO_ISIC4_F" = "Construction",
                           "ECO_ISIC4_G" = "Wholesale & retail trade",
                           "ECO_ISIC4_H" = "Transport & storage",
                           "ECO_ISIC4_I" = "Accommodation & food service",
                           "ECO_ISIC4_J" = "Information & communication",
                           "ECO_ISIC4_K" = "Finance & insurance",
                           "ECO_ISIC4_L" = "Real estate",
                           "ECO_ISIC4_M" = "Professional & technical",
                           "ECO_ISIC4_N" = "Admin & support services",
                           "ECO_ISIC4_O" = "Public administration & defence",
                           "ECO_ISIC4_P" = "Education",
                           "ECO_ISIC4_Q" = "Health & social work",
                           "ECO_ISIC4_R" = "Arts, entertainment & recreation",
                           "ECO_ISIC4_S" = "Other services",
                           "ECO_ISIC4_T" = "Domestic & household labour",
                           "ECO_ISIC4_U" = "Extraterritorial organisations",
                           "ECO_ISIC4_X" = "Not classified"
  ))

isic_table_full <- ISIC4_named |>
  filter(sex == "Female") |>
  mutate(value = round(value, 0),
         share = round(share, 1)) |>
  select(Activity = activity,
         `Women (000s)` = value,
         `% of women's employment` = share) |>
  arrange(desc(`% of women's employment`))   # NAs sort to the bottom automatically

print(isic_table_full, n = 25)
write_csv(isic_table_full, "outputs/iraq_women_isic_full_2021.csv")

# plot

ISIC4_named |>
  filter(sex == "Female", !is.na(activity), !is.na(share)) |>
  ggplot(aes(x = share, y = reorder(activity, share))) +
  geom_col(fill = "#c0504d") +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            hjust = -0.15, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Where employed women work in Iraq, 2021",
    subtitle = "Share of women's total employment, by economic activity (ISIC Rev.4)",
    x = "% of employed women",
    y = NULL,
    caption = "Source: ILO, Employment by economic activity, Iraq LFS 2021. \n Excludes activities with no reportable female employment."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/iraq_women_isic_2021.png",
       width = 9, height = 7, dpi = 300, bg = "white")
