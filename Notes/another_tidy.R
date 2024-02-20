library(tidyverse)
library(janitor)
library(readxl)

dat <- read_xlsx("./Data/messy_bp.xlsx", skip = 3)

bp <- dat %>%
    select(-starts_with("HR"))

n_visits <- dat %>%
    select(starts_with('BP')) %>%
    length()


names(bp)[which(grepl("^BP", names(bp)))] <- paste0('visit',1:n_visits)

bp <- bp %>%
    pivot_longer(starts_with("visit"),
                 names_to = "visit",
                 values_to = "bp",
                 names_prefix = "visit",
                 names_transform = as.numeric)


hr <- dat %>%
    select(-starts_with("BP"))

n_visits <- dat %>%
    select(starts_with('HR')) %>%
    length()


names(hr)[which(grepl("^HR", names(hr)))] <- paste0('visit',1:n_visits)

hr <- hr %>%
    pivot_longer(starts_with("visit"),
                 names_to = "visit",
                 values_to = "hr",
                 names_prefix = "visit",
                 names_transform = as.numeric)

bp <- clean_names(bp)
hr <- clean_names(hr)

df <- full_join(bp, hr)

df <- df %>%
    separate(bp, into = c('systolic', 'diastolic')) %>%
    mutate(systolic_bp = systolic %>% as.numeric(),
           diastolic_bp = diastolic %>% as.numeric()) %>%
    pivot_longer(cols = c("systolic", "diastolic"),
                 names_to = "bp_type",
                 values_to = "bp") %>%
    mutate(race = case_when(
        race == "WHITE" | race == "Caucasian" ~ "White",
        TRUE ~ race)) %>%
    mutate(hispanic = case_when(hispanic == "Hispanic" ~ TRUE,
                                TRUE ~ FALSE)) %>%
    mutate(birthdate = paste(year_birth,month_of_birth,day_birth,sep = "-") %>%
               as.POSIXct())  %>% # posixct universal time code down to ns, knows calendar
    select(-pat_id,-month_of_birth,-day_birth,-year_birth)

