R.version.string
getwd()
install.packages("tidyverse")
no
library(tidyverse)

gs_li_raw <- read_csv("data/gs_li_raw.csv")
dim(gs_li_raw) # rows and columns
glimpse(gs_li_raw) # look at column, type and first values

gs_li_long <- gs_li_raw |> # store the result as a new file and then...
  pivot_longer( # pivot into longer table
    cols = `1957`:`2027`, # which columns should fold up - all the dated ones
    names_to = "year", # transform old column names to a new column 'year'
    values_to = "value", #transform old row contents to new column 'value'
    values_transform = list(value = as.numeric) #read every value as numeric, not 'lgl' which is logical
  )
glimpse(gs_li_long) #provide a glimpse of the data table
dim(gs_li_long) # dim = dimensions of table (rows, columns)

flfp <- gs_li_long |>
  filter( #create a new data table 'flfp' and filter it to the following metadata conditions
    GENDER == "Female", #double '==' asks 'is this equal to that?'
    INDICATOR == "Labor Force Participation, Modeled ILO Estimate, Rate",
    AGE_GROUP == "15+ yrs"
  )
dim(flfp)

flfp |>
  select(year, value) |> # 'filter' chooses rows, 'select' chooses columns
  print(n = 71) # a tibble only shows first 10 rows on default, 'n=71' tells how many rows to show

glimpse(flfp)

flfp_clean <- flfp |>
  select(year, value) |> #creates a new data table with just year and value, as all other rows are same eg 'country' = 'iraq' indicator = 'LFP, Modeled ILO Estimate, rate'
  mutate(year = as.numeric(year)) |> # change 'year' column type to number instead of text 'chr'
  filter(year >= 1990)
dim(flfp_clean)
glimpse(flfp_clean)
print(flfp_clean, n = 38)

ggplot(flfp_clean, aes(x = year, y = value)) +
  geom_line() +
  geom_point() +
  labs(
    title = "FEMALE Labour Force Participation rate in Iraq, 1990-2027",
    x = "Year",
    y = "Participation rate (% of population aged 15+)",
    caption = "Source: ILO modelled estimates, via IMF Gender Data Hub. 2025-2027 are projections."
  ) +
  theme_minimal()

ggsave("outputs/iraq_flfp.png", width = 8, height = 5, dpi = 300)

write_csv(flfp_clean, "outputs/iraq_flfp_clean.csv") # to save the cleaned data to a csv
