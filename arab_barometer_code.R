library(dplyr)
library(readr)

ab <- read_csv("ArabBarometer_WaveVIII_English_v3.csv")

dim(ab)

names(ab)[1]

iraq <- ab |> filter(COUNTRY == 7)
nrow(iraq)

## Make a function to look at all questions and multi-variables

summarise_question <- function(df, var, labels, question_text = var) {
  df <- df |>
    mutate(label = labels[as.character(.data[[var]])])   # look up the label for each code
  df <- df |>
    mutate(label = factor(label, levels = labels))        # make it an ordered factor
  df <- df |>
    filter(!is.na(label))                                  # drop rows never asked (skip pattern)
  
  df |>
    group_by(label) |>                                     # split into one group per response category
    summarise(
      n = n(),                                              # plain count of rows in each group
      weighted_n = sum(WT),                                 # sum of weights in each group
      .groups = "drop"                                      # ungroup afterward (housekeeping, avoids a console message)
    ) |>
    mutate(pct = round(100 * weighted_n / sum(weighted_n), 1)) |>  # weighted percentage
    mutate(question = question_text) |>
    mutate(variable = var)
}

summarise_multi <- function(df, prefix, items, question_text = prefix) {
  results <- tibble()                                                # start empty
  
  for (suffix in names(items)) {                                     # loop over every item number
    col <- paste0(prefix, "_", suffix)                                # build the column name, e.g. "Q412A_1"
    
    n_selected <- sum(df[[col]] == 1, na.rm = TRUE)                    # raw count who selected it
    n_asked <- sum(!is.na(df[[col]]))                                  # raw count who were asked
    
    weighted_n_selected <- sum(df$WT[df[[col]] == 1], na.rm = TRUE)    # weighted count who selected it
    weighted_n_asked <- sum(df$WT[!is.na(df[[col]])])                  # weighted count who were asked
    
    pct <- round(100 * weighted_n_selected / weighted_n_asked, 1)      # weighted percentage
    
    row <- tibble(item = items[[suffix]], n = n_selected,
                  weighted_n = weighted_n_selected, pct = pct, variable = col)
    results <- bind_rows(results, row)
  }
  
  results |> mutate(question = question_text)
}


## Write out all question labels (answer responses)

scale_agree4 <- c(                              # shared: strongly agree...strongly disagree (Q601 series)
  "1" = "Strongly agree", "2" = "Agree", "3" = "Disagree", "4" = "Strongly disagree",
  "98" = "Don't know", "99" = "Refused"
)

scale_trust4 <- c(                              # shared: institutional trust (all Q201A/Q201B items)
  "1" = "A great deal of trust", "2" = "Quite a lot of trust",
  "3" = "Not a lot of trust", "4" = "No trust at all",
  "98" = "Don't know", "99" = "Refused"
)

scale_extent4 <- c(                             # shared: great/medium/small/not at all (Q631 only, in our list)
  "1" = "To a great extent", "2" = "To a medium extent",
  "3" = "To a small extent", "4" = "Not at all",
  "98" = "Don't know", "99" = "Refused"
)
# NOTE: Q431_2 looks similar but says "limited extent", not "medium extent" —
# real wording difference in the source questionnaire, so it gets its own
# vector below rather than reusing this one.

scale_yn <- c("1" = "Yes", "2" = "No", "98" = "Don't know", "99" = "Refused")  # shared: Q104, Q104C

scale_likely4 <- c(                             # shared: job-competition items (Q869F, Q869D, Q869E)
  "1" = "Very likely", "2" = "Somewhat likely",
  "3" = "Somewhat unlikely", "4" = "Very unlikely",
  "98" = "Don't know", "99" = "Refused"
)

scale_support4 <- c(                            # shared: Q916_1, Q916_2, Q916_3 (domestic worker rights)
  "1" = "Strongly support", "2" = "Somewhat support",
  "3" = "Somewhat oppose", "4" = "Strongly oppose",
  "98" = "Don't know", "99" = "Refused"
)

scale_idp_decision <- c(                        # shared: QIRQ2A and QIRQ2B (identical response options)
  "1" = "IDPs themselves", "2" = "Citizens from the local host community",
  "3" = "The local government", "4" = "The national government",
  "5" = "International humanitarian organizations",
  "90" = "Other", "98" = "Don't know", "99" = "Refused"
)

scale_irq3_rights <- c(                         # shared: QIRQ3_1 and QIRQ3_2 (identical response options)
  "1" = "Should be granted rights",
  "2" = "Only if reintegration program completed",
  "3" = "Should not be granted rights",
  "98" = "Don't know", "99" = "Refused"
)

# Shared: Q624_5A, Q624_5B. NOTE: code "97" appears in the raw data range
# but is never given a label in the printed questionnaire text (only 1-4,
# 98, 99 are defined there) — labelled as undocumented rather than guessed.

scale_household4 <- c(
  "1" = "Female head of household", "2" = "Male head of household",
  "3" = "Household heads equally responsible", "4" = "Others are responsible",
  "97" = "[undocumented code]", "98" = "Don't know", "99" = "Refused"
)

scale_freedom4 <- c(                            # shared: all six Q628 items
  "1" = "Men more than women", "2" = "Women more than men",
  "3" = "Men and women have equal freedom", "4" = "Neither men nor women have freedom",
  "97" = "Not applicable", "98" = "Don't know", "99" = "Refused"
)

# Shared: Q604A_NT_1, Q604A_NT_3, Q604B_NT_1, Q604B_NT_3. Same "97"
# undocumented-code situation as scale_household4 above.

scale_obstacle4 <- c(
  "1" = "Constitutes an obstacle to a large degree",
  "2" = "Constitutes an obstacle to a moderate degree",
  "3" = "Constitutes an obstacle to a small degree",
  "4" = "Constitutes no obstacle at all",
  "97" = "[undocumented code]", "98" = "Don't know", "99" = "Refused"
)

scale_widespread4 <- c(                         # shared: Q627_1, Q627_2, Q627_3
  "1" = "Very widespread", "2" = "Fairly widespread",
  "3" = "Fairly rare", "4" = "Very rare",
  "97" = "Women do not face harassment in this country",
  "98" = "Don't know", "99" = "Refused"
)

qirq1_labels <- c(                                    # named vector: code (as text) maps to a label
  "1" = "Strongly support repealing the law",
  "2" = "Somewhat support repealing the law",
  "3" = "Somewhat oppose repealing the law",
  "4" = "Strongly oppose repealing the law",
  "98" = "Don't know",
  "99" = "Refused"
)

q409_labels <- c(                               # unique: internet use frequency
  "1" = "Throughout the day", "2" = "At least once daily",
  "3" = "Several times a week", "4" = "Once a week",
  "5" = "Less than once a week", "6" = "Do not use the Internet",
  "98" = "Don't know", "99" = "Refused"
)

q424_labels <- c(                               # unique: hours/day on social media
  "1" = "Not at all", "2" = "0-2 hours", "3" = "3-5 hours",
  "4" = "6-9 hours", "5" = "10 hours or more",
  "98" = "Don't know", "99" = "Refused"
)

q432b_labels <- c(                              # unique: main influencer topic
  "1" = "Art and culture", "2" = "Beauty and fashion", "3" = "Education",
  "4" = "Food and cooking", "5" = "Health and healthcare",
  "6" = "Politics and reform", "7" = "Religion",
  "8" = "Sports and recreation", "9" = "Technology",
  "90" = "Other", "98" = "Don't know", "99" = "Refused"
)

q431_2_labels <- c(                             # unique: says "limited extent", not "medium" — see note above
  "1" = "To a great extent", "2" = "To a limited extent",
  "3" = "To a small extent", "4" = "Not at all",
  "98" = "Don't know", "99" = "Refused"
)

q412a_items <- c(                          # named vector: item number -> platform name
  "1" = "Facebook", "2" = "Twitter/X", "3" = "Instagram", "4" = "YouTube",
  "5" = "WhatsApp", "6" = "Telegram", "7" = "Snapchat", "8" = "Viber",
  "9" = "Clubhouse", "10" = "Signal", "11" = "TikTok", "12" = "Reddit",
  "13" = "Mastodon", "14" = "BeReal",
  "90" = "Other", "98" = "Don't know", "99" = "Refused"
)

q432_items <- c(                                            # named vector: item number -> item label
  "1" = "Like/share their updates",
  "2" = "Subscribe to their accounts/channels",
  "3" = "Comment or ask questions about their posts",
  "4" = "Try products/services they recommend",
  "5" = "Adopt their political, religious, or cultural views",
  "97" = "Do not interact with social media influencers",
  "98" = "Don't know", "99" = "Refused"
)

q103_labels <- c(                               # unique: generalised interpersonal trust (not institutional)
  "1" = "Most people can be trusted",
  "2" = "Must be very careful dealing with other people",
  "98" = "Don't know", "99" = "Refused"
)

q276_labels <- c(                               # unique: perceived government responsiveness
  "1" = "Very responsive", "2" = "Largely responsive",
  "3" = "Not very responsive", "4" = "Not responsive at all",
  "98" = "Don't know", "99" = "Refused"
)

q1017_labels <- c(                              # unique: remittances frequency
  "1" = "Yes, monthly", "2" = "Yes, a few times a year",
  "3" = "Yes, once a year", "4" = "We do not receive anything",
  "98" = "Don't know", "99" = "Refused"
)

q626_labels <- c(                               # unique: biggest childcare problem
  "1" = "Not widely available", "2" = "Not affordable", "3" = "Poor quality",
  "4" = "Socially unacceptable to use",
  "98" = "Don't know", "99" = "Refused"
)

q625_labels <- c(                              # named vector: code -> label, for Q625
  "1" = "Increased",
  "2" = "Stayed the same",
  "3" = "Decreased",
  "4" = "It was never a problem",
  "98" = "Don't know",
  "99" = "Refused"
)

q630_labels <- c(                               # unique: perceived % agreeing men are better leaders
  "1" = "0-20%", "2" = "21-40%", "3" = "41-60%", "4" = "61-80%", "5" = "81-100%",
  "98" = "Don't know", "99" = "Refused"
)

q1002_labels <- c("1" = "Male", "2" = "Female")

q1003_labels <- c(                              # unique: highest education level
  "1" = "No formal education", "2" = "Elementary", "3" = "Preparatory/Basic",
  "4" = "Secondary", "5" = "Mid-level diploma/professional/technical",
  "6" = "BA", "7" = "MA and above",
  "98" = "Don't know", "99" = "Refused"
)

# NOTE: the printed questionnaire text for Q1005 only defines codes 1-6, 90,
# and 99 — no "98" is documented. A "98" code is nonetheless present in the
# fielded data. Labelled "Don't know" on the assumption that 98 = Don't know
# is Arab Barometer's standard convention everywhere else — an assumption,
# not a documented fact.

q1005_labels <- c(
  "1" = "Employed", "2" = "Self-employed", "3" = "Retired",
  "4" = "Housewife", "5" = "Student", "6" = "Unemployed/looking for work",
  "90" = "Other", "98" = "Don't know [assumed, undocumented]", "99" = "Refused"
)

q1010_labels <- c(                              # unique: marital/social status
  "1" = "Single/Bachelor", "2" = "Living with a partner", "3" = "Engaged",
  "4" = "Married", "5" = "Divorced", "6" = "Separated", "7" = "Widowed",
  "99" = "Refused"
)

q1010b1_labels <- c("1" = "Yes", "2" = "No", "99" = "Refused")  # unique: has children (no DK option in text)

q13_labels <- c("1" = "Urban", "2" = "Rural")   # unique: no DK/refused option in text

governorate_labels <- c(                        # unique: Iraq governorate codes (Appendix 1)
  "70001" = "Baghdad", "70002" = "Salahaddin", "70003" = "Diyala",
  "70004" = "Wasit", "70005" = "Maysan", "70006" = "Basra",
  "70007" = "Dhi Qar", "70008" = "Muthana", "70009" = "Qadisiyah",
  "70010" = "Babylon", "70011" = "Karbala", "70012" = "Najaf",
  "70013" = "Anbar", "70014" = "Nineveh", "70015" = "Dohuk",
  "70016" = "Erbil", "70017" = "Kirkuk", "70018" = "Sulaymaniyah"
)

q104a_2_items <- c(                                     # named vector: item number -> reason for wanting to emigrate
  "1" = "Economic reasons", "2" = "Political reasons",
  "3" = "Religious reasons", "4" = "Security reasons",
  "5" = "Education opportunities", "6" = "Reunite with family",
  "7" = "Corruption",
  "90" = "Other", "98" = "Don't know", "99" = "Refused"
)

q104b_items <- c(                                       # named vector: item number -> destination country
  "1" = "Saudi Arabia", "2" = "United Arab Emirates", "3" = "Qatar",
  "4" = "Bahrain", "5" = "Kuwait", "6" = "Oman", "7" = "Egypt",
  "8" = "Jordan", "9" = "Lebanon", "10" = "Morocco", "11" = "Algeria",
  "12" = "Tunisia", "13" = "Turkey", "14" = "United States", "15" = "Canada",
  "16" = "United Kingdom", "17" = "Eastern Europe", "18" = "France",
  "19" = "Germany", "20" = "Spain", "21" = "Italy",
  "22" = "Other Western European countries", "23" = "Sub-Saharan Africa",
  "24" = "China", "25" = "Australia",
  "90" = "Other", "98" = "Don't know", "99" = "Refused"
)

q622c_irq_items <- c(                                    # named vector: item number -> workplace barrier (WOMEN)
  "1" = "Lack of available jobs", "2" = "Low wages",
  "3" = "Lack of legal right/protection",
  "4" = "Lack of flexibility in working hours",
  "5" = "Lack of childcare options",
  "6" = "Lack of means of transportation",
  "7" = "Lack of skills or relevant education",
  "8" = "Bias against women in hiring",
  "9" = "It is considered socially unacceptable",
  "97" = "None of these are challenges", "98" = "Don't know", "99" = "Refused"
)

# NOTE: the source questionnaire's printed text for item 8 of this question
# literally repeats "Bias against women in hiring" — the same wording used
# for Q622C_IRQ above. Since this whole question is about barriers for MEN,
# that reads as a copy-paste error in the source document. Labelled "men"
# below for internal consistency, but this is my correction of an apparent
# error, not a verbatim transcription — worth checking against Arab
# Barometer's official codebook if you have access to one.

q622e_irq_items <- c(                                    # named vector: item number -> workplace barrier (MEN)
  "1" = "Lack of available jobs", "2" = "Low wages",
  "3" = "Lack of legal right/protection",
  "4" = "Lack of flexibility in working hours",
  "5" = "Lack of childcare options",
  "6" = "Lack of means of transportation",
  "7" = "Lack of skills or relevant education",
  "8" = "Bias against men in hiring [see note above]",
  "9" = "It is considered socially unacceptable",
  "97" = "None of these are challenges", "98" = "Don't know", "99" = "Refused"
)

q629_items <- c(                                         # named vector: item number -> who could help
  "1" = "She will not be able to receive assistance",
  "2" = "A female member of the family",
  "3" = "A male member of the family",
  "4" = "The local police",
  "5" = "A clinic or hospital",
  "6" = "A local organization",
  "98" = "Don't know", "99" = "Refused"
)

## Section B matrix block

section_b_vars <- list(                                                              # list of questions to run
  list(var = "Q103",      labels = q103_labels,   question = "Generalised interpersonal trust"),
  list(var = "Q201A_1",   labels = scale_trust4,   question = "Trust: Government (Council of Ministers)"),
  list(var = "Q201A_2",   labels = scale_trust4,   question = "Trust: Courts and legal system"),
  list(var = "Q201A_3",   labels = scale_trust4,   question = "Trust: Parliament (Council of Representatives)"),
  list(var = "Q201A_5",   labels = scale_trust4,   question = "Trust: Local government"),
  list(var = "Q201A_41",  labels = scale_trust4,   question = "Trust: Regional government"),
  list(var = "Q201A_7",   labels = scale_trust4,   question = "Trust: Civil society organisations"),
  list(var = "Q201A_31C", labels = scale_trust4,   question = "Trust: Prime Minister / Head of Government"),
  list(var = "Q201B_6",   labels = scale_trust4,   question = "Trust: Armed forces (the army)"),
  list(var = "Q201B_4",   labels = scale_trust4,   question = "Trust: Police"),
  list(var = "Q201B_13",  labels = scale_trust4,   question = "Trust: Religious leaders"),
  list(var = "Q201B_12",  labels = scale_trust4,   question = "Trust: Iraqi Islamic Party"),
  list(var = "Q201B_14",  labels = scale_trust4,   question = "Trust: Sadrist Movement / Shiite Nationalist Movement"),
  list(var = "Q201B_15",  labels = scale_trust4,   question = "Trust: Tribal leaders"),
  list(var = "Q276",      labels = q276_labels,    question = "Perceived government responsiveness to what people want")
)

section_b_table <- tibble()                                     # start with an empty table

for (item in section_b_vars) {                                  # loop over each question in the list
  row <- summarise_question(iraq, item$var, item$labels, item$question)  # run the function for this one
  section_b_table <- bind_rows(section_b_table, row)             # add its results to the growing table
}

section_b_table   # view the combined long table — 15 questions stacked, ~90 rows total

trust_matrix <- section_b_table |>                                          # take the long table, then...
  filter(question != "Generalised interpersonal trust",                    # drop Q103 (different scale)
         question != "Perceived government responsiveness to what people want") |>  # drop Q276 (different scale)
  select(question, label, pct) |>                                          # keep just what we need
  pivot_wider(names_from = label, values_from = pct)                       # now spread — all rows share one scale

trust_matrix



## Save all questions into singular CSV file
master_singles <- list(
  # Section A
  list(section = "A", var = "Q409", labels = q409_labels, question = "Frequency of internet use"),
  list(section = "A", var = "Q424", labels = q424_labels, question = "Hours per day spent on social media"),
  list(section = "A", var = "Q432B", labels = q432b_labels, question = "Main topic followed influencers discuss"),
  list(section = "A", var = "Q431_2", labels = q431_2_labels, question = "Concern: social media platforms censoring own online activity"),
  
  # Section B
  list(section = "B", var = "Q103", labels = q103_labels, question = "Generalised interpersonal trust"),
  list(section = "B", var = "Q201A_1", labels = scale_trust4, question = "Trust: Government (Council of Ministers)"),
  list(section = "B", var = "Q201A_2", labels = scale_trust4, question = "Trust: Courts and legal system"),
  list(section = "B", var = "Q201A_3", labels = scale_trust4, question = "Trust: Parliament (Council of Representatives)"),
  list(section = "B", var = "Q201A_5", labels = scale_trust4, question = "Trust: Local government"),
  list(section = "B", var = "Q201A_41", labels = scale_trust4, question = "Trust: Regional government"),
  list(section = "B", var = "Q201A_7", labels = scale_trust4, question = "Trust: Civil society organisations"),
  list(section = "B", var = "Q201A_31C", labels = scale_trust4, question = "Trust: Prime Minister / Head of Government"),
  list(section = "B", var = "Q201B_6", labels = scale_trust4, question = "Trust: Armed forces (the army)"),
  list(section = "B", var = "Q201B_4", labels = scale_trust4, question = "Trust: Police"),
  list(section = "B", var = "Q201B_13", labels = scale_trust4, question = "Trust: Religious leaders"),
  list(section = "B", var = "Q201B_12", labels = scale_trust4, question = "Trust: Iraqi Islamic Party"),
  list(section = "B", var = "Q201B_14", labels = scale_trust4, question = "Trust: Sadrist Movement / Shiite Nationalist Movement"),
  list(section = "B", var = "Q201B_15", labels = scale_trust4, question = "Trust: Tribal leaders"),
  list(section = "B", var = "Q276", labels = q276_labels, question = "Perceived government responsiveness to what people want"),
  
  # Section C
  list(section = "C", var = "Q104", labels = scale_yn, question = "Ever thought about emigrating"),
  list(section = "C", var = "Q104C", labels = scale_yn, question = "Would consider leaving without required papers"),
  list(section = "C", var = "Q1017", labels = q1017_labels, question = "Household receives remittances from abroad"),
  list(section = "C", var = "Q916_1", labels = scale_support4, question = "Support: domestic workers always keep own passport"),
  list(section = "C", var = "Q916_2", labels = scale_support4, question = "Support: domestic workers guaranteed one day off/week"),
  list(section = "C", var = "Q916_3", labels = scale_support4, question = "Support: domestic workers have own bank account for salary"),
  list(section = "C", var = "Q869F", labels = scale_likely4, question = "Perceived likelihood of losing job opportunity to a migrant"),
  list(section = "C", var = "QIRQ2A", labels = scale_idp_decision, question = "Who should decide on return of IDPs living in camps"),
  list(section = "C", var = "QIRQ2B", labels = scale_idp_decision, question = "Who should decide on return of IDPs not living in camps"),
  list(section = "C", var = "QIRQ3_1", labels = scale_irq3_rights, question = "Citizenship rights for children of ISIS-affiliated parents"),
  list(section = "C", var = "QIRQ3_2", labels = scale_irq3_rights, question = "Education rights for children of ISIS-affiliated parents"),
  
  # Section D
  list(section = "D", var = "Q601_3", labels = scale_agree4, question = "Men are better at political leadership than women"),
  list(section = "D", var = "Q601_4", labels = scale_agree4, question = "University education more important for males than females"),
  list(section = "D", var = "Q601_5", labels = scale_agree4, question = "Men and women should have equal work opportunities"),
  list(section = "D", var = "Q601_13", labels = scale_agree4, question = "A woman can reject a family-arranged marriage without her consent"),
  list(section = "D", var = "Q601_13B", labels = scale_agree4, question = "Men and women should have equal say in choosing who they marry"),
  list(section = "D", var = "Q601_18", labels = scale_agree4, question = "A man should have final say in all family decisions"),
  list(section = "D", var = "Q601_18B", labels = scale_agree4, question = "A man and a woman should have equal say in family decisions"),
  list(section = "D", var = "Q601_21A", labels = scale_agree4, question = "There should be a minimum number of parliamentary seats reserved for women"),
  list(section = "D", var = "Q601_21B", labels = scale_agree4, question = "There should be a minimum number of cabinet positions reserved for women"),
  list(section = "D", var = "Q869D", labels = scale_likely4, question = "Perceived likelihood of losing job to an equally/less qualified MALE employee"),
  list(section = "D", var = "Q869E", labels = scale_likely4, question = "Perceived likelihood of losing job to an equally/less qualified FEMALE employee"),
  list(section = "D", var = "Q626", labels = q626_labels, question = "Biggest problem with childcare options"),
  list(section = "D", var = "Q624_5A", labels = scale_household4, question = "Who decides how much to spend on food"),
  list(section = "D", var = "Q624_5B", labels = scale_household4, question = "Who decides what types of food to buy"),
  list(section = "D", var = "Q628_2A", labels = scale_freedom4, question = "Freedom of choice: to pursue higher education"),
  list(section = "D", var = "Q628_2B", labels = scale_freedom4, question = "Freedom of choice: what to study in higher education"),
  list(section = "D", var = "Q628_3A", labels = scale_freedom4, question = "Freedom of choice: to get a job"),
  list(section = "D", var = "Q628_3B", labels = scale_freedom4, question = "Freedom of choice: what type of job to take"),
  list(section = "D", var = "Q628_4A", labels = scale_freedom4, question = "Freedom of choice: to get married"),
  list(section = "D", var = "Q628_4B", labels = scale_freedom4, question = "Freedom of choice: who to marry"),
  list(section = "D", var = "Q604A_NT_1", labels = scale_obstacle4, question = "Obstacle to female relative's marriage: she does not pray"),
  list(section = "D", var = "Q604A_NT_3", labels = scale_obstacle4, question = "Obstacle to female relative's marriage: lower family social status"),
  list(section = "D", var = "Q604B_NT_1", labels = scale_obstacle4, question = "Obstacle to male relative's marriage: he does not pray"),
  list(section = "D", var = "Q604B_NT_3", labels = scale_obstacle4, question = "Obstacle to male relative's marriage: lower family social status"),
  list(section = "D", var = "Q627_1", labels = scale_widespread4, question = "Harassment of women widespread: in the workplace"),
  list(section = "D", var = "Q627_2", labels = scale_widespread4, question = "Harassment of women widespread: on the street by strangers"),
  list(section = "D", var = "Q627_3", labels = scale_widespread4, question = "Harassment of women widespread: in the home by family members"),
  list(section = "D", var = "Q625", labels = q625_labels, question = "Change in abuse/violence against women in the community (past 12 months)"),
  list(section = "D", var = "Q631", labels = scale_extent4, question = "Women in political leadership advances the women's rights agenda"),
  list(section = "D", var = "Q630", labels = q630_labels, question = "Perceived % of citizens agreeing men are better political leaders"),
  list(section = "D", var = "QIRQ1", labels = qirq1_labels, question = "Support for repealing the law allowing rapists to marry their victims"),
  
  # Demographics
  list(section = "Demographics", var = "Q1002", labels = q1002_labels, question = "Gender"),
  list(section = "Demographics", var = "Q1003", labels = q1003_labels, question = "Highest level of education"),
  list(section = "Demographics", var = "Q1005", labels = q1005_labels, question = "Employment status"),
  list(section = "Demographics", var = "Q1010", labels = q1010_labels, question = "Current marital/social status"),
  list(section = "Demographics", var = "Q1010B1", labels = q1010b1_labels, question = "Has children"),
  list(section = "Demographics", var = "Q13", labels = q13_labels, question = "Urban/rural"),
  list(section = "Demographics", var = "Q1001A", labels = governorate_labels, question = "Governorate")
)

singles_results <- tibble()                                          # start empty

for (item in master_singles) {                                       # loop over every question in the list
  row <- summarise_question(iraq, item$var, item$labels, item$question)  # run the function for this one
  row <- row |> mutate(section = item$section)                       # tag it with its section
  singles_results <- bind_rows(singles_results, row)                 # add it to the growing table
}


master_multis <- list(
  list(section = "A", prefix = "Q412A",     items = q412a_items,     question = "Social media platforms actively used"),
  list(section = "A", prefix = "Q432",      items = q432_items,      question = "Interactions with social media influencers followed"),
  list(section = "C", prefix = "Q104A_2",   items = q104a_2_items,   question = "Reasons for considering emigration"),
  list(section = "C", prefix = "Q104B",     items = q104b_items,     question = "Country considered as emigration destination"),
  list(section = "D", prefix = "Q622C_IRQ", items = q622c_irq_items, question = "Barriers to workplace entry for WOMEN in Iraq"),
  list(section = "D", prefix = "Q622E_IRQ", items = q622e_irq_items, question = "Barriers to workplace entry for MEN in Iraq"),
  list(section = "D", prefix = "Q629",      items = q629_items,      question = "Who could help a woman facing violence in the community")
)

multis_results <- tibble()                                             # start empty

for (spec in master_multis) {                                          # renamed item -> spec, avoids the clash
  row <- summarise_multi(iraq, spec$prefix, spec$items, spec$question) # run the function for this one
  row <- row |> mutate(section = spec$section)                        # tag it with its section
  multis_results <- bind_rows(multis_results, row)                    # add it to the growing table
}


singles_results <- singles_results |> rename(response = label)   # rename label -> response
multis_results  <- multis_results  |> rename(response = item)    # rename item -> response

all_results <- bind_rows(
  singles_results |> mutate(type = "single"),   # tag these rows as single-response
  multis_results  |> mutate(type = "multi")     # tag these rows as multi-select
)

all_results <- all_results |>
  select(section, type, variable, question, response, n, pct)   # put columns in a sensible order

write_csv(all_results, "arab_barometer_code_results.csv")   # write it out as a CSV

