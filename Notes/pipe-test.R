library(tidyverse)


iris %>%
    pluck("Species") %>%
    stringr::str_to_title() %>%
    unique()

iris %>%
    pluck("Sepal.Length") %>%
    round(0) %>%
    max()

rnorm(100,0,5) %>%
    abs() %>%
    mean()

seq(0,100,0.01) %>%
    round(1) %>%
    median()
