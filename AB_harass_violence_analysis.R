library(tidyverse)
library(scales)
library(dplyr)

harass_vio_raw <- read_csv("AB_harass_violence_data.csv")


## HARASSMENT AGAINST WOMEN ##

harass_clean <- harass_vio_raw |>
  filter(variable %in% c("Q627_1", "Q627_2", "Q627_3")) |>
  select(variable, question, response, respondent_pct = pct) |>
  filter((!(response == "Don't know"|
            response == "Refused"))) |>
  mutate(question = case_when(
    question == "Harassment of women widespread: in the workplace" ~ "In the workplace",
    question == "Harassment of women widespread: on the street by strangers" ~ "On the street by strangers",
    question == "Harassment of women widespread: in the home by family members" ~ "In the home by family members"
  )) |>
  mutate(response = case_when(
    response == "Very widespread" ~ "Very widespread",
    response == "Fairly widespread" ~ "Fairly widespread",
    response == "Fairly rare" ~ "Fairly rare",
    response == "Very rare" ~ "Very rare",
    response == "Women do not face harassment in this country" ~ "Women do not face harassment"))
harass_clean


ggplot(harass_clean, aes(x = question, y = respondent_pct, fill = fct_inorder(response))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Perceived harassment of women in various settings, Iraq",
    subtitle = "% of respondents rating the spread of work, public and home harassment",
    x = NULL, 
    y = "Percentage of respondents",
    fill = NULL,
    caption = "Source: Q(627_1, _2, _3), Arab Barometer Wave VIII, 2024. \nNote: Single-choice response. Don't know and refused excluded from denominators.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("AB_Iraq_harassment_women.png", width = 8, height = 5, dpi = 300)


## CHANGE IN ABUSE / VIOLENCE ##

abuse_change <- harass_vio_raw |>
  filter(variable == "Q625") |>
  select(variable, question, response, respondent_pct = pct) |>
  filter(!response == "Don't know")

abuse_change

ggplot(abuse_change, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Perceived change in abuse / violence against women in the community",
    subtitle = "12 months 2021-2022, Iraq",
    x = NULL, 
    y = "Percentage of respondents",
    caption = "Source: Q625, Arab Barometer Wave VIII, 2024. \nNote: Single-choice response. Don't know excluded from denominator.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

ggsave("AB_Iraq_change_violence.png", width = 8, height = 5, dpi = 300)


## REPEAL RAPIST MARRIAGE LAW ##

rapist_law <- harass_vio_raw |>
  filter(variable == "QIRQ1") |>
  select(variable, question, response, respondent_pct = pct) |>
  filter(!(response == "Don't know" |
            response == "Refused"))

ggplot(rapist_law, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Support for repealing the law allowing rapists to marry their victims, Iraq",
    x = NULL, 
    y = "Percentage of respondents",
    caption = "Source: QIRQ1, Arab Barometer Wave VIII, 2024. \nNote: Single-choice response. Don't know excluded from denominator.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

ggsave("AB_Iraq_rapist_law_support.png", width = 8, height = 5, dpi = 300)



## HELP WOMEN COMMUNITY VIOLENCE ##

violence_help <- harass_vio_raw |>
  filter(variable %in% c(
    "Q629_1", "Q629_2", "Q629_3", "Q629_4", "Q629_5", "Q629_6")) |>
  select(variable, question, response, respondent_pct = pct)

ggplot(violence_help, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Who in the community could help a women facing violence, Iraq",
    x = NULL, 
    y = "Percentage of respondents",
    caption = "Source: Q629, Arab Barometer Wave VIII, 2024. \nNote: Multi-choice response. Don't know and refused excluded from denominator.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 20, hjust = 1),
        plot.margin = margin(b = 10, l = 60)) +
  coord_cartesian(clip = "off")

ggsave("AB_Iraq_help_violence.png", width = 8, height = 5, dpi = 300)
