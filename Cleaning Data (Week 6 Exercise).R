library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)

setwd("C:/Users/gitte/OneDrive - University of the Virgin Islands/ArcGis Class Folder/Cleaning Data Week 6/data")

mangrove <- read_excel(
  file.choose("messy_mangrove_data.xlsx"),
  col_types = "text"
) %>%
  clean_names()

glimpse(mangrove)
View(mangrove)

mangrove <- mangrove %>%
  rename(
    prop_id = 1,
    location = 2,
    treatment = 3
  )

mangrove_long <- mangrove %>%
  pivot_longer(
    cols = -c(prop_id, location, treatment),
    names_to = "date",
    values_to = "growth"
  )

unique(mangrove_long$date)

View(mangrove_long)


mangrove_clean <- mangrove_long %>%
  mutate(
    prop_id = str_trim(prop_id),
    location = str_to_title(str_squish(location)),
    
    treatment = recode(
      str_to_lower(str_squish(treatment)),
      "ctrl" = "Control",
      "control" = "Control",
      "t1" = "Treatment 1",
      "treatment 1" = "Treatment 1",
      "t2" = "Treatment 2",
      "treatment 2" = "Treatment 2",
      .default = NA_character_
    ),
    
    date = as.Date(
      as.numeric(str_extract(date, "\\d{5}")),
      origin = "1899-12-30"
    ),
    
    growth = str_trim(growth),
    
    status = case_when(
      str_to_upper(growth) == "DEAD" ~ "Dead",
      is.na(growth) |
        str_to_lower(growth) %in% c("missing", "n/a", "") ~ "Missing",
      TRUE ~ "Measured"
    ),
    
    growth_cm = suppressWarnings(
      as.numeric(str_remove(growth, regex("cm$", ignore_case = TRUE)))
    )
  ) %>%
  select(prop_id, location, treatment, date, growth_cm, status)

View(mangrove_clean)


# Histogram of mangrove growth

ggplot(mangrove_clean, aes(x = growth_cm)) +
  geom_histogram(
    binwidth = 5,
    fill = "forestgreen",
    color = "black"
  ) +
  labs(
    title = "Distribution of Mangrove Propagule Growth",
    x = "Propagule Growth (cm)",
    y = "Frequency"
  ) +
  theme_minimal()
