#geom grob

library(tidyverse)
library(ggimage)
library(gganimate)
library(patchwork)
library(gapminder)

str(gapminder)
df = gapminder
mycountries <-  c("Venezuela", "Rwanda", "Nepal", "Iraq", "United States")

p <- gapminder %>%
    mutate(mycountries = case_when(country %in% mycountries~country)) %>%
    ggplot(aes(x= gdpPercap, y = lifeExp,color = continent, scale = "free")) +
    geom_point(aes(size=pop)) +
    geom_text(aes(label = mycountries))
p
p +
    transition_time(time = year) +
    labs(title = "Year: {frame_time}")
# save plots ####
anim_save("./Notes/gapminder_animation.gif")
ggsave("./Notes/plot_example.png")

mycountries <-  c("Venezuela", "Rwanda", "Nepal", "Iraq", "United States")

p2 <- p + facet_wrap(~continent)

p.dark <-
    p + theme_dark()
# patchwork ####
p.dark + p + plot_annotation("MAIN TITLE") +
    plot_layout(guides = "collect")

p2 / p + plot_annotation("Comparison")
