library(tidyverse)
library(scales)
library(dplyr)

employ_raw <- read_csv("AB_employment_data.csv")

glimpse(employ_raw)
unique(employ_raw$question)

## EQUAL WORK OPPORTUNITIES ##

Q601_5_equal_work <- employ_raw |>
  filter(variable == "Q601_5") |>
  select(variable, question, response, respondent_pct = pct)

print(equal_work)

ggplot(Q601_5_equal_work, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = label_number(suffix = "%" )) +
  labs(
    title = "Response to 'men and women should have equal work opportunities', Iraq",
    x = "Single-choice response",
    y = "Percentage of respondents",
    caption = "Source: Q601_5, Arab Barometer Wave VIII, 2024"
  ) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("AB_Iraq_equal_work_opps.png", width = 8, height = 5, dpi = 300)


## LOSING JOB ##

Q869D_lose_job_male <- employ_raw |>
  filter(variable == "Q869D") |>
  select(variable, response, respondent_pct = pct)

Q869E_lose_job_female <- employ_raw |>
  filter(variable == "Q869E") |>
  select(variable, response, respondent_pct = pct)

male_female <- 
  bind_rows(Q869D_lose_job_male, Q869E_lose_job_female) |>
  filter(!response == "Don't know") |>
  mutate(
    variable = case_when(
      variable == "Q869D" ~ "Male", 
      variable == "Q869E" ~ "Female"
    ),
    response = factor(
      response,
      levels = c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely"),
      labels = c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")
    ))

ggplot(male_female, aes(x = fct_inorder(response), y = respondent_pct, fill = fct_inorder(variable))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3) +
  scale_fill_manual(values = c("Male" = "#0072B2", "Female" = "#E69F00")) +
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Perceived likelihood of losing job",
    subtitle = "to equally / less qualified male or female employee, Iraq",
    x = NULL, 
    y = "Percentage of respondents",
    fill = NULL,
    caption = "Source: Q(869D, 869E), Arab Barometer Wave VIII, 2024. Don't know excluded from denominator.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))

ggsave("AB_Iraq_lose_job_gender_likely.png", width = 8, height = 5, dpi = 300)


## BARRIERS TO ENTRY ##

Q622c_barriers_female <- employ_raw |> #Barriers to workplace entry for WOMEN in Iraq
  filter(
      variable == "Q622C_IRQ_1" |
      variable == "Q622C_IRQ_2" |
      variable == "Q622C_IRQ_3" |
      variable == "Q622C_IRQ_4" |
      variable == "Q622C_IRQ_5" |
      variable == "Q622C_IRQ_6" |
      variable == "Q622C_IRQ_7" |
      variable == "Q622C_IRQ_8" |
      variable == "Q622C_IRQ_9" |
      variable == "Q622C_IRQ_97" |
      variable == "Q622C_IRQ_98" |
      variable == "Q622C_IRQ_99"
         ) |>
  select(variable, response, respondent_pct = pct) |>
  filter(!(response == "Don't know" | response == "Refused")) |>
  filter(!is.na(response)) |>
  mutate(
    variable = case_when(
      variable == "Q622C_IRQ_1" ~ "Female",
      variable == "Q622C_IRQ_2" ~ "Female",
      variable == "Q622C_IRQ_3" ~ "Female",
      variable == "Q622C_IRQ_4" ~ "Female",
      variable == "Q622C_IRQ_5" ~ "Female",
      variable == "Q622C_IRQ_6" ~ "Female",
      variable == "Q622C_IRQ_7" ~ "Female",
      variable == "Q622C_IRQ_8" ~ "Female",
      variable == "Q622C_IRQ_9" ~ "Female",
      variable == "Q622C_IRQ_97" ~ "Female",
      variable == "Q622C_IRQ_98" ~ "Female",
      variable == "Q622C_IRQ_99" ~ "Female"),
    response = factor(
     response,
      levels = c("Lack of available jobs",
               "Low wages",
               "Lack of legal right/protection",
               "Lack of flexibility in working hours",  
               "Lack of childcare options",
               "Lack of means of transportation",      
               "Lack of skills or relevant education",
               "Bias against women in hiring", 
               "It is considered socially unacceptable",
               "None of these are challenges"),
      labels = c("Lack of available jobs",
               "Low wages",
               "Lack of legal right/protection",
               "Lack of flexibility in working hours",  
               "Lack of childcare options",
               "Lack of means of transportation",      
               "Lack of skills or relevant education",
               "Bias against women in hiring",   
               "It is considered socially unacceptable",
               "None of these are challenges")))

Q622e_barriers_male <- employ_raw |> #Barriers to workplace entry for MEN in Iraq
  filter(
      variable == "Q622E_IRQ_1" |
      variable == "Q622E_IRQ_2" |
      variable == "Q622E_IRQ_3" |
      variable == "Q622E_IRQ_4" |
      variable == "Q622E_IRQ_5" |
      variable == "Q622E_IRQ_6" |
      variable == "Q622E_IRQ_7" |
      variable == "Q622E_IRQ_8" |
      variable == "Q622E_IRQ_9" |
      variable == "Q622E_IRQ_97" |
      variable == "Q622E_IRQ_98" |
      variable == "Q622E_IRQ_99"
  ) |>
  select(variable, response, respondent_pct = pct) |>
  filter(!(response == "Don't know" | response == "Refused")) |>
  filter(!is.na(response)) |>
  mutate(
    variable = case_when(
      variable == "Q622E_IRQ_1" ~ "Male",
      variable == "Q622E_IRQ_2" ~ "Male",
      variable == "Q622E_IRQ_3" ~ "Male",
      variable == "Q622E_IRQ_4" ~ "Male",
      variable == "Q622E_IRQ_5" ~ "Male",
      variable == "Q622E_IRQ_6" ~ "Male",
      variable == "Q622E_IRQ_7" ~ "Male",
      variable == "Q622E_IRQ_8" ~ "Male",
      variable == "Q622E_IRQ_9" ~ "Male",
      variable == "Q622E_IRQ_97" ~ "Male",
      variable == "Q622E_IRQ_98" ~ "Male",
      variable == "Q622E_IRQ_99" ~ "Male"
    ),
    response = factor(
      response,
      levels = c("Lack of available jobs",
                 "Low wages",
                 "Lack of legal right/protection",
                 "Lack of flexibility in working hours",  
                 "Lack of childcare options",
                 "Lack of means of transportation",      
                 "Lack of skills or relevant education",
                 "Bias against men in hiring [see note above]",
                 "It is considered socially unacceptable",
                 "None of these are challenges"),
      labels = c("Lack of available jobs",
                 "Low wages",
                 "Lack of legal right/protection",
                 "Lack of flexibility in working hours",  
                 "Lack of childcare options",
                 "Lack of means of transportation",      
                 "Lack of skills or relevant education",
                 "Bias against men in hiring",
                 "It is considered socially unacceptable",
                 "None of these are challenges")
    ))

male_female_barriers <- 
  bind_rows(Q622c_barriers_female, Q622e_barriers_male) |>
  mutate(
    response = if_else(
      response %in% c("Bias against men in hiring",
                      "Bias against women in hiring"),
      "Bias in hiring",
      as.character(response)))

ggplot(male_female_barriers, aes(x = response, y = respondent_pct, fill = fct_inorder(variable))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3) +
  scale_fill_manual(values = c("Male" = "#0072B2", "Female" = "#E69F00")) +
  scale_y_continuous(limits = c(0, 75), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Barriers to workplace entry for men and women in Iraq",
    x = NULL, 
    y = "Percentage of respondents",
    fill = NULL,
    caption = "Source: Q(622C_, 622E_), Arab Barometer Wave VIII, 2024. Multiple-choice response. Don't know / refused excluded from denominator.") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("AB_Iraq_barrier_job_gender.png", width = 8, height = 5, dpi = 300)
