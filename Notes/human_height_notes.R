# SETUP ####
library(tidyverse)
library(readxl)
library(measurements)
library(easystats)

# DATA ####
path <- "./Data/human_heights.xlsx"
dat <- read_xlsx(path)

# CLEAN ####
dat <-
dat %>%
  pivot_longer(everything(),
               names_to = "sex",
               values_to = "height") %>%
  separate(height, into = c("feet","inches"),convert = TRUE) %>%
  mutate(inches = (feet*12) + inches) %>%
  mutate(cm=conv_unit(inches, from='in',to='cm'))

# distribution plot
# density with x and y, only x
dat %>%
  ggplot(aes(x=cm,fill=sex)) +
  geom_density(alpha=.5)

# sample
# population
# never prove, only falsify

# modeling ####
# lhs (left hand side) ~ rhs (right hand side)
# x ~ y
# left - measurment, right - factor
# outcome (as a function of ~) some predictor
t.test(dat$cm ~ factor(dat$sex))

# pvalue assumes nulll hypothesis is true
# surprise value
# smaller the number - the more surprising if there is no difference

mod <- glm(data = dat, formula = cm~sex)
summary(mod)
# estimates = coefficients, y = mx + b
# y = output
# m = slope (Intercept)
# b = y intercept

# report ####
report(mod)
performance(mod)

# mpg example ####
str(mpg)


mpg %>%
    ggplot(aes(x = displ, y = cty)) +
    geom_point() +
    geom_smooth(method = "glm")
mod2 <- glm(data = mpg, formula = cty ~ displ)  # displ is engine size

report(mod2)
performance(mod2)

# for every unit of displacement you loose 2
# -2.6 * disp + 26 = mpg
# point of modeling = to predict

# RMSE ####
# the higher it is the more off your model is
performance::check_model(mod2)

# HW ####
# go through modeling in week 9
# mlu linear regression house prices

# sample size matters ####
data.frame(A = rnorm(1000000, mean = 0, sd = 1),
           B = rnorm(1000000, mean = 5, sd = 1)) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density()

