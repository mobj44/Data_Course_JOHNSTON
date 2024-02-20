library(tidyverse)
library(palmerpenguins)
library(ggimage)
library(GGally)


ggpairs(penguins)
str(penguins)

plot1 <- penguins %>%
    ggplot() +
    geom_density2d_filled(aes(x = bill_depth_mm, y = bill_length_mm, color = island, fill = species)) +
    geom_image(aes(x = bill_depth_mm, y = bill_length_mm, image= image_path), size = 0.09) +
    theme(panel.background = element_rect(fill = 'limegreen'),
          panel.grid = element_line(color = 'yellow', linewidth = 3),
          plot.background = element_rect('red'))
plot1

penguin_plot <- penguins %>%
    ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
    geom_image(aes(x = bill_depth_mm, y = bill_length_mm, image= image_path), size = 0.09) +
    theme(panel.background = element_rect(fill = 'limegreen'),
          panel.grid.major = element_line(color = 'yellow', linewidth = 5),
          panel.grid.minor = element_line(color = 'yellow', linewidth = 5),
          plot.background = element_rect('firebrick1'),
          axis.text.x = element_text(color = 'firebrick3', angle = 90),
          axis.text.y = element_text(color = 'firebrick3', angle = 180),
          axis.title.x = element_text(color = 'purple'),
          axis.title.y = element_text(color = 'purple'),
          plot.title = element_text(hjust = .5, color = 'purple')) +
    scale_y_continuous(breaks = c(40,50,60), labels = c('smol', 'okrrr', 'big chongus')) +
    scale_x_continuous(breaks = c(15, 17.5, 20.0), labels = c('smol', 'okrrr', 'big chongus')) +
    labs(title = "Face size", x = "length", y = "depth")

ggsave('./penguin_plot.png', dpi = 80)

penguins$image_path <- penguins$species %>%
    as.character() %>%
    paste0('./penguin_images/', ., '.png')


penguins %>%
    ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
    geom_density_2d_filled() +
    geom_image(aes(image= image_path), size = 0.05)


