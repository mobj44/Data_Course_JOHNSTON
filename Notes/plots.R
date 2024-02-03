library(tidyverse)
library(palmerpenguins)

str(penguins)

penguins %>%
    ggplot(mapping = aes(x = bill_length_mm,
                         y = bill_depth_mm,
                         color = body_mass_g)) +
    geom_point()

penguins %>%
    ggplot(aes(x = species,
               y = body_mass_g)) +
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.1, alpha = 0.3, aes(color = sex))

penguins %>% # distributions
    ggplot(aes(x=body_mass_g,
               fill = species)) +
    geom_density(alpha=.25)


df <- read_delim("./Data/DatasaurusDozen.tsv")

df %>%
    group_by(dataset) %>%
    summarize(meanx = mean(x),
              sdx = sd(x),
              minx = min(x),
              medianx = median(x))
df %>%
    ggplot(aes(x=x,y=y)) +
    geom_point() +
    facet_wrap(~dataset)

str(penguins)

penguins %>%
    filter(!is.na(sex)) %>%
    ggplot(mapping = aes(x = bill_depth_mm,
                         y = body_mass_g,
                         color = species,
                         shape = sex)) +
    geom_point(alpha = .85) +
    facet_wrap(~ island) + # free allows scale to be different for each graph
    labs(x = "Bill Depth (mm)",
         y = "Body Mass (g)",
         color = "Species",
         shape = "Sex")


# exploratory plots for visualizing at data ####
library(GGally)
ggpairs(penguins) # works best with fewer than 10


# pre explore Gapminder ####
library(gapminder)
str(gapminder)

gapminder %>%
    filter(year == '2007' & grepl("^U", country)) %>%
    ggplot(aes(x = country,
               y = pop,
               color = lifeExp,
               fill = lifeExp)) +
    geom_col()


# Rules ####
# dont hide data
# have a goal
# plot before running stats

# leaflet ####
# addTiles
# addCircleMarkers
