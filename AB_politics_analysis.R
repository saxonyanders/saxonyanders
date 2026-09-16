library(tidyverse)
library(scales)
library(dplyr)

politics_raw <- read_csv("AB_politics_data.csv")

politics_raw
unique(politics_raw$question)


## MALE SUPERIOR POLITICAL LEADERSHIP ##

men_leadership <- politics_raw |>
  filter(variable == "Q601_3" |
           variable == "Q630") |>
  select(variable, question, response, respondent_pct = pct)

men_leadership <- men_leadership |>
  filter(!response == "Don't know") |>
  mutate(
    response = factor(
      response,
      levels = c("Strongly agree", "81-100%", "Agree", "61-80%", "41-60%", "Disagree", "21-40%", "Strongly disagree", "0-20%"),
      labels = c("Strongly agree", "81-100%", "Agree", "61-80%", "41-60%", "Disagree", "21-40%", "Strongly disagree", "0-20%")
    ))

unique(men_leadership$response)

ggplot(men_leadership, aes(x = response, y = respondent_pct, fill = fct_inorder(question))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Opinion of superior male political leadership versus",
    subtitle = "perceived % of other citizens who hold opinion men are superior political leaders, Iraq",
    x = NULL, 
    y = "Percentage of respondents",
    fill = NULL,
    caption = "Source: Q(630, 601_3), Arab Barometer Wave VIII, 2024. Don't know excluded from denominator.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

ggsave("AB_Iraq_men_superior_leadership.png", width = 8, height = 5, dpi = 300)


## WOMEN IN POLITICS ## 

unique(women_politics$question)

women_politics <- politics_raw |>
  filter(variable == "Q601_21A" |
           variable == "Q601_21B") |>
  select(variable, question, response, respondent_pct = pct)

women_politics <- women_politics |>
  filter(!response == "Don't know") |>
  mutate(question = case_when(
    question == "There should be a minimum number of parliamentary seats reserved for women" ~ "Parliamentary seats",
    question == "There should be a minimum number of cabinet positions reserved for women" ~ "Cabinet positions"
    ))

ggplot(women_politics, aes(x = fct_inorder(response), y = respondent_pct, fill = fct_inorder(question))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Opinions of whether there should be minimum numbers of \nparliamentary / cabinet seats reserved for women, Iraq",
    x = NULL, 
    y = "Percentage of respondents",
    fill = NULL,
    caption = "Source: Q601_21A/B, Arab Barometer Wave VIII, 2024. Don't know excluded from denominator.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

ggsave("AB_Iraq_reserved_politics_women.png", width = 8, height = 5, dpi = 300)

