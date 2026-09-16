library(tidyverse)

install.packages("scales")
library(scales)



ab_social_media_raw <- read_csv("AB_social_media_data.csv")

ab_social_media_raw <- ab_social_media_raw |>
  select(type, variable, question, response, respondent_pct = pct)

glimpse(ab_social_media_raw)
print(ab_social_media_raw, n = 52)
unique(ab_social_media_raw$question)


## Q424 & Q409 ##
ab_sm_freq <- ab_social_media_raw |>
  select(variable, Q = question, response, respondent_pct) |>
  filter(variable == "Q409" |
         variable == "Q424"
         )

ab_sm_freq$question <- paste(ab_sm_freq$variable, ab_sm_freq$Q, sep = "/")

ab_sm_freq <- ab_sm_freq |>
  select(question, response, respondent_pct)

Q424_ab_sm_hours <- ab_sm_freq |>
  filter(question == "Q424/Hours per day spent on social media") |>
  select(response, respondent_pct)

Q424_ab_sm_hours

ggplot(Q424_ab_sm_hours, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = label_number(                   
    suffix = "%"
  )) +
  labs(
    title = "Hours per day of social media use, Iraq",
    x = "Single-choice response",
    y = "Percentage of respondents",
    caption = "Source: Q424, Arab Barometer Wave VIII, 2024"
  ) +  
  theme(legend.position = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("AB_Iraq_hours_social_media_use.png", width = 8, height = 5, dpi = 300)


Q409_ab_internet_use <- ab_sm_freq |>
  filter(question == "Q409/Frequency of internet use") |>
  select(response, respondent_pct)

Q409_ab_internet_use

ggplot(Q409_ab_internet_use, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = label_number(suffix = "%" )) +
  labs(
    title = "Frequency of internet use, Iraq",
    x = "Single-choice response",
    y = "Percentage of respondents",
    caption = "Source: Q409, Arab Barometer Wave VIII, 2024"
  ) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("AB_Iraq_internet_use.png", width = 8, height = 5, dpi = 300)


unique(ab_social_media_raw)


## Q432B - topics influencers discuss ##
Q432B_ab_topic_influencer <- ab_social_media_raw |>
  filter(variable == "Q432B") |>
  select(response, respondent_pct)

ggplot(Q432B_ab_topic_influencer, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = label_number(suffix = "%" )) +
  labs(
    title = "Main topic that influencers following discuss, Iraq",
    x = "Single-choice response",
    y = "Percentage of respondents",
    caption = "Source: Q432B, Arab Barometer Wave VIII, 2024"
  ) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("AB_Iraq_influencer_topic.png", width = 8, height = 5, dpi = 300)


## Q431_2 - social media censorship ##
Q431_2_ab_sm_censor <- ab_social_media_raw |>
  filter(variable == "Q431_2") |>
  select(response, respondent_pct)

ggplot(Q431_2_ab_sm_censor, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = label_number(suffix = "%" )) +
  labs(
    title = "Level of concern regarding social media platforms \ncensoring one's own content, Iraq",
    x = "Single-choice response",
    y = "Percentage of respondents",
    caption = "Source: Q431_2, Arab Barometer Wave VIII, 2024"
  ) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("AB_Iraq_sm_censorship.png", width = 8, height = 5, dpi = 300)


## Q412A_ - social media platforms used ##
Q412A_ab_sm_platform <- ab_social_media_raw |>
  filter(variable %in% c(
    "Q412A_1", "Q412A_2",  "Q412A_3",
    "Q412A_4",  "Q412A_5",  "Q412A_6",  
    "Q412A_7",  "Q412A_8", "Q412A_9",  
    "Q412A_10", "Q412A_11", "Q412A_12",
    "Q412A_13", "Q412A_14", "Q412A_90", "Q412A_98", 
    "Q412A_99"
    )) |>
  select(response, respondent_pct)

Q412A_ab_sm_platform

ggplot(Q412A_ab_sm_platform, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = label_number(suffix = "%" )) +
  labs(
    title = "Social media platforms actively used, Iraq",
    x = "Multi-choice response",
    y = "Percentage of respondents",
    caption = "Source: Q412A_1-_99, Arab Barometer Wave VIII, 2024"
  ) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("AB_Iraq_sm_platform_used.png", width = 8, height = 5, dpi = 300)



## Q432_ - interactions with influencers they follow ##
Q432_ab_infl_interact <- ab_social_media_raw |>
  filter(variable %in% c(
    "Q432_1", "Q432_2",  "Q432_3",
    "Q432_4",  "Q432_97", "Q432_98", 
    "Q432_99"
  )) |>
  select(response, respondent_pct)

ggplot(Q432_ab_infl_interact, aes(x = fct_inorder(response), y = respondent_pct)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = label_number(suffix = "%" )) +
  labs(
    title = "How respondents interact with influencers they follow, Iraq",
    x = "Multi-choice response",
    y = "Percentage of respondents",
    caption = "Source: Q432_, Arab Barometer Wave VIII, 2024"
  ) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("AB_Iraq_infl_interact.png", width = 8, height = 5, dpi = 300)
