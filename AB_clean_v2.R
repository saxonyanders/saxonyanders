library(tidyverse)
library(dplyr)

arab_baro_raw <- read_csv("arab_barometer_clean.csv")

glimpse(arab_baro_raw)

unique(arab_baro_raw$question)

ques_var <- arab_baro_raw |>
  select(variable, question) |>
  filter(variable == "Q409" |
               variable == "Q424" |
               variable == "Q432B" |
               variable == "Q431_2" |
               variable == "Q412A_1" |
               variable == "Q412A_2" |
               variable == "Q412A_3" |
               variable == "Q412A_4" |
               variable == "Q412A_5" |
               variable == "Q412A_6" |
               variable == "Q412A_7" |
               variable == "Q412A_8" |
               variable == "Q412A_9" |
               variable == "Q412A_10" |
               variable == "Q412A_11" |
               variable == "Q412A_12" |
               variable == "Q412A_13" |
               variable == "Q412A_14" |
               variable == "Q412A_90" |
               variable == "Q412A_98" |
               variable == "Q412A_99" |
               variable == "Q432_1" |
               variable == "Q432_2" |
               variable == "Q432_3" |
               variable == "Q432_4" |
               variable == "Q432_5" |
               variable == "Q432_97" |
               variable == "Q432_98" |
               variable == "Q432_99" |
             variable == "Q103" |
             variable == "Q201A_1" |  
             variable == "Q201A_2" |
             variable == "Q201A_3" |
             variable == "Q201A_5" |
             variable == "Q201A_41" |
             variable == "Q201A_7" |
             variable == "Q201A_31C" |
             variable == "Q201B_6" |
             variable == "Q201B_4" |
             variable == "Q201B_13" |
             variable == "Q201B_12" |
             variable == "Q201B_14" |
             variable == "Q201B_15" |
             variable == "Q276" |
             variable == "Q104" |
             variable == "Q104C" |
             variable == "Q1017" |
             variable == "Q104A_2_1" |
             variable == "Q104A_2_2" |
             variable == "Q104A_2_3" |
             variable == "Q104A_2_4" |
             variable == "Q104A_2_5" |
             variable == "Q104A_2_6" |
             variable == "Q104A_2_7" |
             variable == "Q104A_2_90" |
             variable == "Q104A_2_98" |
             variable == "Q104A_2_99" |
             variable == "Q104B_1" |
             variable == "Q104B_2" |
             variable == "Q104B_3" |
             variable == "Q104B_4" |
             variable == "Q104B_5" |
             variable == "Q104B_6" |
             variable == "Q104B_7" |
             variable == "Q104B_8" |
             variable == "Q104B_9" |
             variable == "Q104B_10" |
             variable == "Q104B_11" |
             variable == "Q104B_12" |
             variable == "Q104B_13" |
             variable == "Q104B_14" |
             variable == "Q104B_15" |
             variable == "Q104B_16" |
             variable == "Q104B_17" |
             variable == "Q104B_18" |
             variable == "Q104B_19" |
             variable == "Q104B_20" |
             variable == "Q104B_21" |
             variable == "Q104B_22" |
             variable == "Q104B_23" |
             variable == "Q104B_24" |
             variable == "Q104B_25" |
             variable == "Q104B_90" |
             variable == "Q104B_98" |
             variable == "Q104B_99" |
             variable == "Q916_1" |
             variable == "Q869F" |
             variable == "QIRQ2A" |
             variable == "QIRQ2B" |
             variable == "QIRQ3_1" |
             variable == "QIRQ3_2" |
             variable == "Q1002" |
             variable == "Q1003" |
             variable == "Q1005" |
             variable == "Q1010" |
             variable == "Q1010B1" |
             variable == "Q13" |
             variable == "Q1001A" |
             variable == "Q628_3A" |
             variable == "Q628_3B" |
             variable == "Q628_4A" |
             variable == "Q628_4B" |
             variable == "Q628_2A" |
             variable == "Q628_2B" |
             variable == "Q916_2" |
             variable == "Q916_3" |
             variable == "Q601_5" |
             variable == "Q869D" |
             variable == "Q869E" |
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
             variable == "Q622C_IRQ_99" |
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
             variable == "Q622E_IRQ_99" |
             variable == "Q601_4" |
             variable == "Q601_13" |
             variable == "Q601_13B" |
             variable == "Q601_18" |
             variable == "Q601_18B" |
             variable == "Q626" |
             variable == "Q624_5A" |
             variable == "Q624_5B" |
             variable == "Q604A_NT_1" |
             variable == "Q604A_NT_3" |
             variable == "Q604B_NT_1" |
             variable == "Q604B_NT_3" |
             variable == "Q601_3" |
             variable == "Q601_21A" |
             variable == "Q601_21B" |
             variable == "Q631" |
             variable == "Q630" |
             variable == "Q627_1" |
             variable == "Q627_2" |
             variable == "Q627_3" |
             variable == "Q625" |
             variable == "QIRQ1" |
             variable == "Q629_1" |
             variable == "Q629_2" |
             variable == "Q629_3" |
             variable == "Q629_4" |
             variable == "Q629_5" |
             variable == "Q629_6" |
             variable == "Q629_98" |
             variable == "Q629_99"
  )

ab_social <- arab_baro_raw |>
  filter(
          variable == "Q409" |
          variable == "Q424" |
          variable == "Q432B" |
          variable == "Q431_2" |
          variable == "Q412A_1" |
          variable == "Q412A_2" |
          variable == "Q412A_3" |
          variable == "Q412A_4" |
          variable == "Q412A_5" |
          variable == "Q412A_6" |
          variable == "Q412A_7" |
          variable == "Q412A_8" |
          variable == "Q412A_9" |
          variable == "Q412A_10" |
          variable == "Q412A_11" |
          variable == "Q412A_12" |
          variable == "Q412A_13" |
          variable == "Q412A_14" |
          variable == "Q412A_90" |
          variable == "Q412A_98" |
          variable == "Q412A_99" |
          variable == "Q432_1" |
          variable == "Q432_2" |
          variable == "Q432_3" |
          variable == "Q432_4" |
          variable == "Q432_5" |
          variable == "Q432_97" |
          variable == "Q432_98" |
          variable == "Q432_99"
           )
write_csv(ab_social, "AB_social_media_data.csv")


ab_trust <- arab_baro_raw |>
  filter(
      variable == "Q103" |
      variable == "Q201A_1" |  
      variable == "Q201A_2" |
      variable == "Q201A_3" |
      variable == "Q201A_5" |
      variable == "Q201A_41" |
      variable == "Q201A_7" |
      variable == "Q201A_31C" |
      variable == "Q201B_6" |
      variable == "Q201B_4" |
      variable == "Q201B_13" |
      variable == "Q201B_12" |
      variable == "Q201B_14" |
      variable == "Q201B_15" |
      variable == "Q276"
  )
write_csv(ab_trust, "AB_trust_data.csv")

ab_migration <- arab_baro_raw |>
  filter(
      variable == "Q104" |
      variable == "Q104C" |
      variable == "Q1017" |
      variable == "Q104A_2_1" |
      variable == "Q104A_2_2" |
      variable == "Q104A_2_3" |
      variable == "Q104A_2_4" |
      variable == "Q104A_2_5" |
      variable == "Q104A_2_6" |
      variable == "Q104A_2_7" |
      variable == "Q104A_2_90" |
      variable == "Q104A_2_98" |
      variable == "Q104A_2_99" |
      variable == "Q104B_1" |
      variable == "Q104B_2" |
      variable == "Q104B_3" |
      variable == "Q104B_4" |
      variable == "Q104B_5" |
      variable == "Q104B_6" |
      variable == "Q104B_7" |
      variable == "Q104B_8" |
      variable == "Q104B_9" |
      variable == "Q104B_10" |
      variable == "Q104B_11" |
      variable == "Q104B_12" |
      variable == "Q104B_13" |
      variable == "Q104B_14" |
      variable == "Q104B_15" |
      variable == "Q104B_16" |
      variable == "Q104B_17" |
      variable == "Q104B_18" |
      variable == "Q104B_19" |
      variable == "Q104B_20" |
      variable == "Q104B_21" |
      variable == "Q104B_22" |
      variable == "Q104B_23" |
      variable == "Q104B_24" |
      variable == "Q104B_25" |
      variable == "Q104B_90" |
      variable == "Q104B_98" |
      variable == "Q104B_99"
  )
write_csv(ab_migration, "AB_out_migration_data.csv")

# CONTINUE WITH CREATING CSVS FROM QUES_VAR QUESTIONS - 
# MOST REMAINING MIGHT BE FEMALE, THINK NEED TO SPLIT GENERAL SOCIAL OPINIONS FROM 
# GENDER-BASED OPINIONS

ab_immigration <- arab_baro_raw |>
  filter(
    variable == "Q916_1" |
      variable == "Q869F" |
      variable == "QIRQ2A" |
      variable == "QIRQ2B" |
      variable == "QIRQ3_1" |
      variable == "QIRQ3_2"
  )
write_csv(ab_immigration, "AB_immigration_data.csv")

ab_demog <- arab_baro_raw |>
  filter(
    variable == "Q1002" |
      variable == "Q1003" |
      variable == "Q1005" |
      variable == "Q1010" |
      variable == "Q1010B1" |
      variable == "Q13" |
      variable == "Q1001A"
  )
write_csv(ab_demog, "AB_demog_data.csv")


ab_freedom_choice <- arab_baro_raw |>
  filter(
    variable == "Q628_3A" |
      variable == "Q628_3B" |
      variable == "Q628_4A" |
      variable == "Q628_4B" |
      variable == "Q628_2A" |
      variable == "Q628_2B"
  )
write_csv(ab_freedom_choice, "AB_freedom_choice_data.csv")


ab_employment <- arab_baro_raw |>
  filter(
    variable == "Q916_2" |
      variable == "Q916_3" |
      variable == "Q601_5" |
      variable == "Q869D" |
      variable == "Q869E" |
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
      variable == "Q622C_IRQ_99" |
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
  )
write_csv(ab_employment, "AB_employment_data.csv")


ab_marriage <- arab_baro_raw |>
  filter(
    variable == "Q601_4" |
      variable == "Q601_13" |
      variable == "Q601_13B" |
      variable == "Q601_18" |
      variable == "Q601_18B" |
      variable == "Q626" |
      variable == "Q624_5A" |
      variable == "Q624_5B" |
      variable == "Q604A_NT_1" |
      variable == "Q604A_NT_3" |
      variable == "Q604B_NT_1" |
      variable == "Q604B_NT_3"
  )
write_csv(ab_marriage, "AB_marriage_data.csv")


ab_politics <- arab_baro_raw |>
  filter(
    variable == "Q601_3" |
      variable == "Q601_21A" |
      variable == "Q601_21B" |
      variable == "Q631" |
      variable == "Q630"
  )
write_csv(ab_politics, "AB_politics_data.csv")


ab_harass_violence <- arab_baro_raw |>
  filter(
    variable == "Q627_1" |
      variable == "Q627_2" |
      variable == "Q627_3" |
      variable == "Q625" |
      variable == "QIRQ1" |
      variable == "Q629_1" |
      variable == "Q629_2" |
      variable == "Q629_3" |
      variable == "Q629_4" |
      variable == "Q629_5" |
      variable == "Q629_6" |
      variable == "Q629_98" |
      variable == "Q629_99"
  )
write_csv(ab_harass_violence, "AB_harass_violence_data.csv")


unique(ques_var$variable)
