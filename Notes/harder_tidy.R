library(tidyverse)
library(janitor)
library(skimr) # good info about data

df <- read_csv("./Data/Bird_Measurements.csv")

skim(df)

df <- clean_names(df)


keep <- c("family", "species_number", "species_name", "english_name",
          "clutch_size", "egg_mass", "mating_system")

female <- df %>%
    select(keep, starts_with("f_"), -ends_with("_n")) %>%
    mutate(sex = "female")
names(female) <- names(female) %>% str_remove("f_")


male <- df %>%
    select(keep, starts_with("m_"), -ends_with("_n")) %>%
    mutate(sex = "male")
names(male) <- names(male) %>% str_remove("m_")

unsexed <- df %>%
    select(keep, starts_with("u"), -ends_with("_n")) %>%
    mutate(sex = "unsexed")
names(unsexed) <- names(unsexed) %>% str_remove("unsexed_")

clean <- male %>%
    full_join(female) %>%
    full_join(unsexed)
