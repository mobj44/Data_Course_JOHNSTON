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

bp %>%
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

hr %>%
    pivot_longer(starts_with("visit"),
                 names_to = "visit",
                 values_to = "hr",
                 names_prefix = "visit",
                 names_transform = as.numeric)

bp <- clean_names(bp)
hr <- clean_names(hr)


