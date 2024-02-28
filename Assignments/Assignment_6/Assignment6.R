library(tidyverse)
library(janitor)
library(gganimate)

df <- read_csv("../../Data/BioLog_Plate_Data.csv")


df <-
    df %>%
    clean_names() %>%
    pivot_longer(cols = starts_with("hr_"),
                 names_to = "hours",
                 values_to = "absorbance",
                 names_prefix = "hr_",
                 names_transform = as.numeric) %>%
    mutate(type = case_when(sample_id == "Clear_Creek" ~ "Water",
                            sample_id == "Soil_1" ~ "Soil",
                            sample_id == "Waste_Water" ~ "Water",
                            sample_id == "Soil_2" ~ "Soil"))

dilution_plot <- df %>%
    filter(dilution == 0.1) %>%
    ggplot(aes(x = hours,
               y = absorbance,
               color = type)) +
    geom_smooth(se = FALSE) +
    facet_wrap(~substrate) +
    labs(title = "Just dilution 0.1",
         x = "Absorbance",
         y = "Time",
         color = "Type")
dilution_plot
ggsave("./dilution_plot.png")


animated_plot <- df %>%
    filter(substrate == "Itaconic Acid") %>%
    group_by(sample_id, dilution, hours) %>%
    mutate(mean_abs = mean(absorbance)) %>%
    ggplot(aes(x = hours,
               y = mean_abs,
               color = sample_id)) +
    geom_line() +
    facet_wrap(~dilution) +
    transition_reveal(hours) +
    theme_minimal()+
    labs(x = "Time", y = "Mean_absorbance", color = "Sample ID")
animated_plot

anim_save("./abs_plot.gif",animation = animated_plot)


