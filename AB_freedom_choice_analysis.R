library(tidyverse)
library(dplyr)
library(scales)

freedom_choice_raw <- read_csv("AB_freedom_choice_data.csv") |>
  select(variable, question, response, respondent_pct = pct)

freedom_choice <- freedom_choice_raw |>
  filter(!(response == "Don't know"|
         response == "Refused"|
         response == "Not applicable")) |>
  mutate(
         response = case_when(
           response == "Men more than women" ~ "Men more than women",
           response == "Women more than men" ~ "Women more than men",
           response == "Men and women have equal freedom" ~ "Men and women equally",
           response == "Neither men nor women have freedom" ~ "Neither men or women"
         ),
        question = case_when(
          question == "Freedom of choice: to pursue higher education" ~ "To pursue HE",    
          question == "Freedom of choice: what to study in higher education" ~ "What to study in HE",
          question == "Freedom of choice: to get a job" ~ "To get a job",                    
          question == "Freedom of choice: what type of job to take" ~ "What job to take",         
          question == "Freedom of choice: to get married" ~ "To get married",
          question == "Freedom of choice: who to marry" ~ "Who to marry"
        ))

ggplot(freedom_choice, aes(x = fct_inorder(question), y = respondent_pct, fill = fct_inorder(response))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(
    label = round(respondent_pct, 0)), 
    position = position_dodge(width = 0.9),
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.08)), 
                     labels = label_number(suffix = "%")) +
  labs(
    title = "Perceived freedom of choice to make decisions, Iraq",
    subtitle = "% of respondents rating each choice on a gendered scale",
    x = NULL, 
    y = "Percentage of respondents",
    fill = "Freedom of choice",
    caption = "Source: Q628, Arab Barometer Wave VIII, 2024. \nNote: Single-choice response. 'HE' = higher education. Don't know, refused and not applicable excluded from denominators.",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  
ggsave("AB_Iraq_freedom_choice.png", width = 8, height = 5, dpi = 300)
  
