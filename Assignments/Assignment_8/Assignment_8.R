library(modelr)
library(easystats)
library(tidyverse)
library(caret)


# loads the “/Data/mushroom_growth.csv” data set
mushrooms <- read_csv("../../Data/mushroom_growth.csv")

summary(mushrooms)

# creates several plots exploring relationships between the response and predictors
mushrooms %>%
    ggplot(aes(x = Species, y = GrowthRate)) +
    geom_col() +
    facet_wrap(~Light)

mushrooms %>%
    ggplot(aes(x = Humidity, y = GrowthRate)) +
    geom_col() +
    facet_wrap(~Temperature)

mushrooms %>%
    ggplot(aes(x = Species, y = GrowthRate)) +
    geom_col() +
    facet_wrap(~Humidity)

mushrooms %>%
    ggplot(aes(x = GrowthRate, group = factor(Nitrogen), fill = factor(Nitrogen))) +
    geom_density(alpha = .5) +
    facet_wrap(~Species) +
    labs(x = "Growth Rate", y = "Density", fill = "Nitrogen")

# defines at least 4 models that explain the dependent variable “GrowthRate”
id <- caret::createDataPartition(mushrooms$GrowthRate, p = 0.8, list = FALSE)
train <- mushrooms[id,]
test <- mushrooms[-id,]

mod1 <- glm(data = train, GrowthRate ~ Humidity * Temperature * Light * Species)
mod2 <- glm(data = train, GrowthRate ~ Humidity + Temperature + Light + Species)
mod3 <- glm(data = train, GrowthRate ~ Humidity * Light + Temperature + Nitrogen + Species)
mod4 <- glm(data = train, GrowthRate ~ Humidity * Temperature * Light * Species * Nitrogen)

# calculates the mean sq. error of each model
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2) # selects the best model you tried

# adds predictions based on new hypothetical values for the independent variables used in your model
pred = predict(mod4, newdata = test)

hyp_preds <- data.frame(test, pred = pred)

mushrooms$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

fullpreds <- full_join(mushrooms, hyp_preds)

# plots these predictions alongside the real data
fullpreds %>%
    ggplot(aes(x = GrowthRate, fill = PredictionType)) +
    geom_density(alpha = 0.5)

fullpreds %>%
    ggplot(aes(x = Humidity, y = GrowthRate, color = PredictionType)) +
    geom_boxplot()

fullpreds %>%
    ggplot(aes(x = Temperature, y = GrowthRate, fill = PredictionType)) +
    geom_violin()

# linear model
df <- read_csv("../../Data/non_linear_relationship.csv")

df %>%
    mutate(log_response = log(response)) %>%
    ggplot(aes(x = predictor, y = log_response)) +
    geom_point() +
    geom_smooth()

df <- df %>%
    mutate(log_response = log(response))

mod5 <- glm(data = df, formula = log_response ~ predictor)
summary(mod5)

