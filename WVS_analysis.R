library(tidyverse)
library(readxl)

wvs_raw <- read_excel("data/F00013193-WVS_Wave_7_Iraq_Excel_v5.1.xlsx", sheet = "Data")
names(wvs_raw) <- str_extract(names(wvs_raw), "^[^:]+")

wvs <- wvs_raw |>
  select(
    region = N_REGION_ISO,
    sex = Q260, age = Q262,
    education = Q275R, employment = Q279,
    marital = Q273, children = Q274,
    harm_work_mother = Q28, housewife_fulfil = Q32,
    men_prior_job = Q33, wife_breadwinner = Q35,
    pref_sector = MENA_17, looking_for_work = MENA_19
  ) |>
  # negatives -> NA (the critical missing-data step)
  mutate(across(everything(), ~ if_else(.x < 0, NA_real_, .x))) |>
  # decode the variables we'll group by
  mutate(
    sex = recode(sex, `1` = "Male", `2` = "Female"),
    governorate = recode(as.character(region),
                         "368001" = "Al Anbar", "368002" = "Al Basrah",
                         "368006" = "Arbil",    "368007" = "As Sulaymaniyah",
                         "368008" = "Babil",    "368009" = "Baghdad",
                         "368011" = "Dhi Qar",  "368014" = "Kirkuk",
                         "368016" = "Ninawá"),
    region_group = if_else(governorate %in% c("Arbil", "As Sulaymaniyah"),
                           "KRG", "Federal Iraq"),
    education = recode(education, `1` = "Lower", `2` = "Middle", `3` = "Higher")
  )
summary(wvs)

#wvs priority men work
wvs |>
  filter(!is.na(men_prior_job), !is.na(region_group)) |>
  mutate(agrees = men_prior_job %in% c(1, 2)) |>
  group_by(region_group, sex) |>
  summarise(n = n(), pct_agree = round(mean(agrees) * 100, 1), .groups = "drop")

#wvs mother work harms children
wvs |>
  filter(!is.na(harm_work_mother), !is.na(region_group)) |>
  mutate(agrees = harm_work_mother %in% c(1, 2)) |>
  group_by(region_group, sex) |>
  summarise(n = n(), pct_agree = round(mean(agrees) * 100, 1), .groups = "drop")

#wvs housewife is fulfilling
wvs |>
  filter(!is.na(housewife_fulfil), !is.na(region_group)) |>
  mutate(agrees = housewife_fulfil %in% c(1, 2)) |>
  group_by(region_group, sex) |>
  summarise(n = n(), pct_agree = round(mean(agrees) * 100, 1), .groups = "drop")

#wvs wife breadwinner
wvs |>
  filter(!is.na(wife_breadwinner), !is.na(region_group)) |>
  mutate(agrees = wife_breadwinner %in% c(1, 2)) |>
  group_by(region_group, sex) |>what 
  summarise(n = n(), pct_agree = round(mean(agrees) * 100, 1), .groups = "drop")

#wvs table
pct_agree_by <- function(var) {
  wvs |>
    filter(!is.na({{ var }}), !is.na(region_group)) |>
    mutate(agrees = {{ var }} %in% c(1, 2)) |>
    group_by(region_group, sex) |>
    summarise(n = n(), pct_agree = round(mean(agrees) * 100, 1), .groups = "drop")
}

attitudes <- bind_rows(
  pct_agree_by(men_prior_job)    |> mutate(item = "Men should have priority to jobs"),
  pct_agree_by(harm_work_mother) |> mutate(item = "Having a worker mother harms children"),
  pct_agree_by(wife_breadwinner) |> mutate(item = "Problem if wife earns more")
)

print(attitudes, n = 12)

# plot
ggplot(attitudes, aes(x = region_group, y = pct_agree, fill = sex)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct_agree, 0)),
            position = position_dodge(width = 0.9),
            vjust = -0.4, size = 3) +
  facet_wrap(~ item) +
  scale_y_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Gender-role attitudes by region and sex, Iraq",
    subtitle = "% agreeing with each statement (WVS Wave 7, 2018)",
    x = NULL, y = "% agreeing", fill = "Sex",
    caption = "Source: World Values Survey Wave 7 (Iraq). Unweighted. KRG n=69-83 per cell; estimates approximate."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("outputs/iraq_gender_attitudes_region_sex_wvs.png",
       width = 9, height = 7, dpi = 300, bg = "white")


#table
# Build the full table from all four items, keeping n's
attitudes_table <- bind_rows(
  pct_agree_by(men_prior_job)    |> mutate(item = "Men have more right to a job"),
  pct_agree_by(harm_work_mother) |> mutate(item = "Working mother harms children"),
  pct_agree_by(wife_breadwinner) |> mutate(item = "Problem if wife earns more"),
  pct_agree_by(housewife_fulfil) |> mutate(item = "Housewife as fulfilling as paid work*")
) |>
  # combine region + sex into one group label, and pct + n into one cell
  mutate(group = paste(region_group, sex),
         cell  = paste0(round(pct_agree, 0), "% (n=", n, ")")) |>
  select(item, group, cell) |>
  pivot_wider(names_from = group, values_from = cell)

print(attitudes_table)
write_csv(attitudes_table, "outputs/wvs_attitudes_by_region_sex.csv")
