library(survivoR)
library(gganimate)

str(vote_history)

# count the number of times a person is voted per episode
vote_counts <- vote_history %>%
    group_by(season, episode, vote) %>%
    mutate(count = n(), .groups = 'drop')

max_votes_per_ep <- vote_counts %>%
    group_by(season, episode) %>%
    mutate(max_votes = max(count))

p <- max_votes_per_ep %>%
    group_by(season) %>%
    ggplot(aes(x = episode, y = max_votes)) +
    geom_point()

p + transition_states(states = season, transition_length = .1, state_length = 100) +
    labs(title = "Season: {closest_state}", x = "Max Votes", y = "Episode")
anim_save("../project_plot.gif")
