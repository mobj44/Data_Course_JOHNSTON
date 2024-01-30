library(tidyverse)
library(palmerpenguins)

penguins %>% names

# Filter ####
# bill_length_mm > 40
x <- penguins %>%
    filter(bill_length_mm > 40 & sex == "female")
mean(x$body_mass_g)

# find mean body mass of female penguins with long beaks
penguins %>%
    filter(bill_length_mm > 40 & sex == "female") %>%
    pluck("body_mass_g") %>%
    mean

# same but for each species
penguins %>%
    filter(bill_length_mm > 40 & sex == "female") %>%
    group_by(species) %>%
    summarize(mean_body_mass = mean(body_mass_g))

# add min, max, stdev, and n
penguins %>%
    filter(bill_length_mm > 40 & sex == "female") %>%
    group_by(species, island) %>%
    summarize(mean_body_mass = mean(body_mass_g),
              min_body_mass = min(body_mass_g),
              max_body_mass = max(body_mass_g),
              st_body_mass = sd(body_mass_g),
              N = n()) %>%
    arrange(desc(mean_body_mass)) %>%
    write_csv(file = "./Data/penguine_summary.csv")

# find the fattie penguins (body mass > 5000)
# count how  many are male and how many are female
# return the max body mass for male sand females
penguins %>%
    filter(body_mass_g > 5000) %>%
    group_by(sex) %>%
    summarize(N = n(),
              max_body_mass = max(body_mass_g))

# bonus: add new column to penguins that says whether they are fattie
penguins %>%
    mutate(fatties = body_mass_g > 5000)

x <- penguins %>%
    mutate(fatstat = case_when(body_mass_g > 5000 ~ "fattie", body_mass_g <= 5000 ~ "skinny"))
# Plotting ####
x %>%
    filter(!is.na(sex)) %>%
    ggplot(mapping = aes(x = body_mass_g,
                         y = bill_length_mm,
                         color = fatstat)) +
    geom_point() +
    geom_smooth() +
    scale_color_manual(values = c("violet", "skyblue2")) +
    theme_dark() +
    theme(axis.text = element_text(angle = 180, face = 'italic'))
    # scale_color_viridis_d(option = 'plasma', end=.8)

# ggmap gis mapping using ggplot
# google maps with R

## :: is called namespace ####
# basically package

# barplot ####
names(penguins)
penguins %>%
    ggplot(mapping = aes(x = flipper_length_mm, y = body_mass_g, fill = species)) +
    geom_col() # bar chart uses position stack
# position dodge wil put next to eachother

# scatter plot ####
penguins %>%
    ggplot(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
    geom_point()

penguins %>%
    ggplot(mapping = aes(x = flipper_length_mm, fill = species)) +
    geom_bar() # more of histogram, uses stat_count()

penguins %>%
    ggplot(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
    geom_line(aes(group=species))

penguins %>%
    ggplot(mapping = aes(x = flipper_length_mm,
                         y = body_mass_g,
                         color = species,
                         alpha = bill_depth_mm)) +

    geom_path(aes(group=species)) +
    stat_ellipse() +
    geom_point(aes(color=sex)) +
    geom_polygon() +
    geom_hex() +
    geom_bin_2d() +
    geom_boxplot() +
    geom_hline(yintercept = 4500,
               linewidth=25,
               color='magenta',
               linetype = '11',
               alpha=.45) +
    geom_point(color="yellow", aes(alpha=bill_depth_mm)) +
    theme(axis.title = element_text(face = 'italic',
                                    size = 12,
                                    angle = 30),
          legend.background = element_rect(fill = 'hotpink',
                                           color = 'blue',
                                           linewidth = 5))
    #geom_image(image='~/Documents/pengin.jpeg')
# alpha = transparency

library(ggimage)

names(penguins)
