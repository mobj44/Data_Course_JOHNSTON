library(tidyverse)
library(easystats)

# 1.Read in the unicef data
df <- read_csv("unicef-u5mr.csv")

# 2. Get it into tidy format
df <- df %>%
    pivot_longer(cols = -c(CountryName, Continent, Region),
                 names_to = "Year",
                 values_to = "U5MR",
                 names_prefix = "U5MR.",
                 names_transform = as.numeric)
# 3. Plot each country’s U5MR over time
df %>%
    ggplot(aes(x = Year, y = U5MR, group = CountryName)) +
    geom_line() +
    facet_wrap(~Continent)

# 4. Save this plot as LASTNAME_Plot_1.png
ggsave(filename = "./JOHNSTON_Plot_1.png")

# 5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year
df %>%
    group_by(Year, Continent) %>%
    mutate(Mean_U5MR = mean(U5MR, na.rm = TRUE)) %>%
    ggplot(aes(x = Year, y = Mean_U5MR, color = Continent, group = Continent)) +
    geom_point(size = 1) +
    geom_line(aes(y = Mean_U5MR), size = 2.5,na.rm = TRUE)

# 6. Save that plot as LASTNAME_Plot_2.png
ggsave(filename = "./JOHNSTON_Plot_2.png")

# 7. Create three models of U5MR
mod1 <- glm(data = df, formula = U5MR ~ Year)
mod2 <- glm(data = df, formula = U5MR ~ Year + Continent)
mod3 <- glm(data = df, formula = U5MR ~ Year * Continent)

# 8. Compare the three models with respect to their performance
compare_performance(mod1, mod2, mod3)
# Model 3 is the best, it has the highest R2, the lowest RMSE, and the
# lowest AIC.

# 9. Plot the 3 models’ predictions
df$mod1 <- predict(mod1, df)
df$mod2 <- predict(mod2, df)
df$mod3 <- predict(mod3, df)

df <- df %>%
    pivot_longer(starts_with("mod"), names_to = "Model",values_to = "Prediction")

df %>%
    ggplot(aes(x = Year,
               y = Prediction,
               color = Continent)) +
    geom_smooth(method = "lm") +
    facet_wrap(~Model) +
    theme_bw() +
    labs(title = "Model predictions",
         x = "Year",
         y = "Predicted U5MR")

ggsave("model_predictions.png")

# 10.
ecuador_data <- df %>%
    filter(CountryName == "Ecuador")

mod4 <- glm(U5MR ~ poly(Year, 2),
            data = ecuador_data)

predicted_ecuador_2020 <- predict(mod4, newdata = data.frame(Year = 2020))

actual <- 13

difference <- actual - predicted_ecuador_2020

results <- data.frame(
    Model = "mod4",
    Prediction = predicted_ecuador_2020,
    Reality = actual,
    Difference = difference
)
results
