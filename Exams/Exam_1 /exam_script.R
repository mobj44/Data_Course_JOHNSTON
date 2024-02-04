library(tidyverse)
library(stringr)

# I. Read the cleaned_covid_data.csv file into an R data frame. ####
df <- read_csv("./data/cleaned_covid_data.csv")

# II. Subset the data set to just show states that begin with “A” and save this ####
# as an object called A_states.
A_states <- df %>% 
  filter(grepl('^A',Province_State))

# III. Create a plot of that subset showing Deaths over time, with a separate ####
# facet for each state.
A_states %>% 
  ggplot(mapping = aes(x = Last_Update,
                       y = Deaths,
                       color=Province_State)) +
  
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) + 
  facet_wrap(~ Province_State,scales = 'free') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "Date")

# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio ####
# for each state and save this as a new data frame object called 
# state_max_fatality_rate. 
state_max_fatality_rate <- df %>% 
  filter(!is.na(Case_Fatality_Ratio)) %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))


# V. Use that new data frame from task IV to create another plot. ####
state_max_fatality_rate %>% 
  ggplot(aes(x = reorder(Province_State, -Maximum_Fatality_Ratio),
             y = Maximum_Fatality_Ratio)) +
  geom_col(fill = 'deepskyblue3') + 
  theme_minimal() +
  theme(axis.text = element_text(angle = 90)) +
  labs(x = "State", y = "Maximum Fatality Ratio")


# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the ####
# entire US over time
cumulative_deaths <- df %>% 
  group_by(Last_Update) %>% 
  summarize(total_deaths = sum(Deaths, na.rm = TRUE)) %>% 
  mutate(deaths_over_time = cumsum(total_deaths))

cumulative_deaths %>% 
  ggplot(mapping= aes(x = Last_Update,
                 y = deaths_over_time)) + 
  geom_point(size = .2) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "Date",
       y = "Deaths")
