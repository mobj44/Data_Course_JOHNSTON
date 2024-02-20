library(tidyverse)
library(readxl)
library(janitor)

df <- read_csv("./Data/wide_income_rent.csv")

# plot rent prices for each state
# state on the x-axis, rent on y-axis, bar chart

names(df)
df2 <- t(df) %>% as.data.frame()

df2 <- df2[-1,]
df2$state <- row.names(df2)
names(df2) <- c("income", "rent", "state")


df %>%
    pivot_longer(-variable, names_to = "state",
                 values_to = "amount") %>%
    pivot_wider(names_from = variable,
                values_from = amount) %>%
    ggplot(aes(x = state, y = rent)) +
    geom_col() +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = .5, size = 10))

# row is a single observation

table1
table2 %>%
    pivot_wider(names_from = type,
                values_from = count)

table3 %>%
    separate(rate, into = c('cases', 'population'))

x <- table4a %>%
    pivot_longer(cols = -country,
                 names_to = 'year',
                 values_to = 'cases')
y <- table4b %>%
    pivot_longer(cols = -country,
                 names_to = 'year',
                 values_to = 'population')
full_join(x,y)

table5 %>%
    separate(rate, into =  c('cases', 'population'), convert = TRUE) %>%
    mutate(year = paste0(century,year) %>%  as.numeric()) %>%
    select(-century)

# Reading excel ####
dat <- read_xlsx("./Data/messy_bp.xlsx", skip = 3)

names(dat)
clean_names(dat)


n_visits <- dat %>%
    select(starts_with('BP')) %>%
    length()
1:n_visits

# which ####
which(grepl("^BP", names(bp)))
paste0('visit',1:n_visits)

names(dat)[8:10]

bp <- dat %>%
    select(-starts_with("HR")) %>%
    pivot_longer(cols = starts_with("BP"),
                 names_to = "visit",
                 values_to = "bp") %>%
    mutate(visit = case_when(visit == "BP...8" ~ 1,
                             visit == "BP...10" ~ 2,
                             visit == "BP...12" ~ 3)) %>%
    separate(bp, into = c('systolic_bp', 'diastolic_bp'))

hr <- dat %>%
    select(-starts_with("BP")) %>%
    pivot_longer(cols = starts_with("HR"),
                 names_to = "visit",
                 values_to = "hr") %>%
    mutate(visit = case_when(visit == "HR...9" ~ 1,
                             visit == "HR...11" ~ 2,
                             visit == "HR...13" ~ 3))
df <- full_join(bp,hr)

# cleanup column names to be not evil
df <- df %>%
    clean_names()

# cleanup race column and datatime
df <- df %>%
    mutate(race = case_when(
        race == "WHITE" | race == "Caucasian" ~ "White",
        TRUE ~ race)) %>%
    mutate(birthdata = paste(year_birth,month_of_birth,day_birth,sep = "-") %>%
               as.POSIXct())  %>% # posixct universal time code down to ns, knows calendar
    mutate(systolic_bp = systolic_bp %>% as.numeric(),
           diastolic_bp = diastolic_bp %>% as.numeric()) %>%
    select(-pat_id,-month_of_birth,-day_birth,-year_birth) %>%
    mutate(hispanic = case_when(hispanic == "Hispanic" ~ TRUE,
                                TRUE ~ FALSE)) %>%
    pivot_longer(cols = c("systolic_bp", "diastolic_bp"), names_to = "bp_type", values_to = "bp")

df %>%
    ggplot(aes(x = visit, y = bp,color = bp_type)) +
    geom_path() +
    facet_wrap(~race)


# start with fresh R scritp
# dont look at this code
# use BP data and clean from scratch

