## DTM's national estimates of households displaced since January 2014, 
## by wave of displacement and assessment round, with returns to areas of origin over the same period.

library(tidyverse)
library(ggrepel)

idp_raw <- read_csv("iom_idp_all.csv", show_col_types = FALSE)
ret_raw <- read_csv("iom_returnee_all.csv", show_col_types = FALSE)

glimpse(idp_raw)
glimpse(ret_raw)

all_periods <- c("Pre-June 2014", "June-July 2014", "August 2014", "Post September 2014",
                 "Post April 2015", "Post March 2016", "Post October 2016", "July 2017",
                 "Jan 2019", "Jan 2020", "Jan 2021", "Jan 2022", "Jan 2023", "Jan 2024")

idp_places <- idp_raw %>%
  select(round_number, round_date, count_unit, place_id,
         idp_households, idp_individuals, all_of(all_periods))

idp_places <- idp_places %>%
  mutate(multiplier = idp_individuals / idp_households)

idp_places <- idp_places %>%
  mutate(across(all_of(all_periods),
                ~ if_else(count_unit == "individuals", .x / multiplier, .x)))

idp_places <- idp_places %>%
  distinct(round_number, place_id, .keep_all = TRUE)

ret_places <- ret_raw %>%
  select(round_number, round_date, count_unit, place_id,
         returnee_households, returnee_individuals, all_of(all_periods))

ret_places <- ret_places %>%
  mutate(multiplier = returnee_individuals / returnee_households)

ret_places <- ret_places %>%
  mutate(across(all_of(all_periods),
                ~ if_else(count_unit == "individuals", .x / multiplier, .x)))

ret_places <- ret_places %>%
  distinct(round_number, place_id, .keep_all = TRUE)

idp_cohort <- idp_places %>%
  pivot_longer(cols = all_of(all_periods),
               names_to = "displacement_period",
               values_to = "count")

idp_cohort <- idp_cohort %>%
  group_by(round_number, round_date, displacement_period) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

idp_cohort <- idp_cohort %>%
  mutate(status = "Still displaced")                     

ret_cohort <- ret_places %>%
  pivot_longer(cols = all_of(all_periods),
               names_to = "displacement_period",
               values_to = "count")

ret_cohort <- ret_cohort %>%
  group_by(round_number, round_date, displacement_period) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

ret_cohort <- ret_cohort %>%
  mutate(status = "Returned")                             

cohort <- bind_rows(idp_cohort, ret_cohort)

cohort <- cohort %>%
  filter(round_number != 5)                               # round 5 period totals exceed its origin totals

cohort <- cohort %>%
  mutate(displacement_period = factor(displacement_period, levels = all_periods))
cohort <- cohort %>%
  arrange(round_date, displacement_period, status)

cohort_wide <- cohort %>%
  pivot_wider(names_from = status,
              values_from = count)

cohort_wide <- cohort_wide %>%
  rename(still_displaced = `Still displaced`,
         returned = Returned)

cohort_wide <- cohort_wide %>%
  arrange(round_date, displacement_period)

print(cohort_wide, n = 200)

## Plotting data
wave_starts <- tibble(
  displacement_period = all_periods,
  wave_start = as.Date(c("2014-01-01", "2014-06-01", "2014-08-01", "2014-09-01",
                         "2015-04-01", "2016-03-01", "2016-10-01", "2017-07-01",
                         "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                         "2023-01-01", "2024-01-01"))
)

# attach each wave's start date so we can hide periods before they existed
cohort_plot <- cohort_wide %>%
  left_join(wave_starts, by = "displacement_period")

# keep only observations from rounds that postdate the wave itself
cohort_plot <- cohort_plot %>%
  filter(round_date >= wave_start)

# round 8's third category merges August and Post September, so those two cells are not comparable
cohort_plot <- cohort_plot %>%
  filter(!(round_number == 8 &
             displacement_period %in% c("August 2014", "Post September 2014")))

cohort_plot <- cohort_plot %>%
  mutate(displacement_period = factor(displacement_period, levels = all_periods))

cohort_plot <- cohort_plot %>%
  mutate(wave = if_else(displacement_period %in% c("Jan 2019", "Jan 2020", "Jan 2021",
                                                   "Jan 2022", "Jan 2023", "Jan 2024"),
                        "2019 onwards", as.character(displacement_period)))

wave_levels <- c("Pre-June 2014", "June-July 2014", "August 2014", "Post September 2014",
                 "Post April 2015", "Post March 2016", "Post October 2016", "July 2017",
                 "2019 onwards")

cohort_plot <- cohort_plot %>%
  mutate(wave = factor(wave, levels = wave_levels))

cohort_plot <- cohort_plot %>%
  group_by(round_date, wave) %>%
  summarise(still_displaced = sum(still_displaced, na.rm = TRUE), .groups = "drop")

cohort_plot <- cohort_plot %>%
  filter(still_displaced > 0)

wave_colours <- c(
  "Pre-June 2014"       = "#000000",   # black
  "June-July 2014"      = "#56B4E9",   # sky blue
  "August 2014"         = "#D55E00",   # vermillion, the key line
  "Post September 2014" = "#0072B2",   # dark blue
  "Post April 2015"     = "#009E73",   # green
  "Post March 2016"     = "#CC79A7",   # pink
  "Post October 2016"   = "#E69F00",   # orange
  "July 2017"           = "#7570B3",   # purple
  "2019 onwards"        = "#999999"    # grey
)

label_end <- cohort_plot %>%
  group_by(wave) %>%
  filter(round_date == max(round_date)) %>%
  ungroup()

ggplot(cohort_plot, aes(x = round_date, y = still_displaced, colour = wave)) +
  geom_line(linewidth = 0.7) +                          
  geom_point(size = 1.2) +                             
  geom_text_repel(data = label_end,                     # labels at the right-hand end
                  aes(label = wave),
                  hjust = 0, nudge_x = 120,             # push right of the last point
                  direction = "y", size = 3,
                  segment.size = 0.2, min.segment.length = 0.5) +
  scale_colour_manual(values = wave_colours, guide = "none") +         
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(breaks = seq(as.Date("2014-01-01"), as.Date("2024-01-01"), by = "2 years"), date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.15))) + 
  coord_cartesian(xlim = c(as.Date("2014-06-01"), as.Date("2026-04-01")),
                  clip = "off") +                        # crop the view, keep the labels
  labs(x = NULL,
       y = "Households",
       title = "Households remaining displaced, by wave of displacement  \nIraq 2014 - 2024",
       caption = "Source: IOM DTM Iraq Master Lists, rounds 8-134. \nFigures are households; individual counts in the source data are a fixed multiplier and are not observed.") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave("iraq_general_displacement_by_wave_2014_2024.png", width = 10, height = 6, dpi = 300)


## CSV of results

wave_peak <- cohort_wide %>%
  group_by(displacement_period) %>%
  filter(still_displaced == max(still_displaced, na.rm = TRUE)) %>%
  ungroup()

wave_peak <- wave_peak %>%
  select(displacement_period,
         peak_date = round_date,
         peak_displaced = still_displaced)

wave_latest <- cohort_wide %>%
  group_by(displacement_period) %>%
  filter(round_date == max(round_date)) %>%
  ungroup()

wave_latest <- wave_latest %>%
  select(displacement_period,
         latest_date = round_date,
         latest_displaced = still_displaced,
         latest_returned = returned)

wave_summary <- wave_peak %>%
  left_join(wave_latest, by = "displacement_period")

wave_summary <- wave_summary %>%
  mutate(share_retained = latest_displaced / peak_displaced)

wave_summary <- wave_summary %>%
  arrange(displacement_period)

print(wave_summary)

write_csv(wave_summary, "iraq_general_displacement_by_wave_2014_2024.csv")


## Diverging bar chart
wave_bars <- wave_summary %>%
  select(displacement_period, latest_displaced, latest_returned)

wave_bars <- wave_bars %>%
  mutate(wave = if_else(displacement_period %in% c("Jan 2019", "Jan 2020", "Jan 2021",
                                                   "Jan 2022", "Jan 2023", "Jan 2024"),
                        "2019 onwards", as.character(displacement_period)))

wave_bars <- wave_bars %>%
  group_by(wave) %>%
  summarise(latest_displaced = sum(latest_displaced, na.rm = TRUE),
            latest_returned = sum(latest_returned, na.rm = TRUE),
            .groups = "drop")

wave_bars <- wave_bars %>%
  mutate(wave = factor(wave, levels = wave_levels))

wave_bars <- wave_bars %>%
  pivot_longer(cols = c(latest_displaced, latest_returned),
               names_to = "measure",
               values_to = "households")

wave_bars <- wave_bars %>%
  mutate(households = if_else(measure == "latest_returned",
                              -households, households))

wave_bars <- wave_bars %>%
  mutate(measure = if_else(measure == "latest_displaced",
                           "Remains displaced", "Returned to date"))

ggplot(wave_bars, aes(x = wave, y = households, fill = measure)) +
  geom_col(width = 0.7) +                                    # bars above and below zero
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_text(aes(label = scales::comma(abs(households)),
                vjust = if_else(households >= 0, -0.4, 1.3)),
            size = 2.8, colour = "grey20", show.legend = FALSE) +
  scale_y_continuous(labels = NULL,
                     breaks = NULL,
                     expand = expansion(mult = 0.08)) +   
  scale_fill_manual(values = c("Remains displaced" = "#D55E00",
                               "Returned to date" = "#0072B2")) +
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       title = "Households remaining displaced vs returned by wave, \nIraq 2014 - 2024") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))

ggsave("Iraq_general_displacement_by_wave_diverging_2014_2024.png", width = 11, height = 7, dpi = 300)
