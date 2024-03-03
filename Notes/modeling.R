library(tidyverse)
library(easystats)
library(MASS)

# use mpg data
# build a model that predicts cty as a function of displ

# mod1 ####
mod1 <- glm(data = mpg, cty~displ)

mod <- glm(data = mpg, cty~poly(displ,2))

mod1$coefficients # intercept and slope (influence of displ)
# as displ goes up 1 unit city goes down -2.63 mpg

mod1$residuals # the distance each point is from the predictor line

mod1$formula # specific class in R, accesses formula

names(mpg)


mpg %>%
    ggplot(aes(x = displ, y = cty)) +
    geom_smooth(method = "glm")

# mod2 ####
mod2 <- glm(data = mpg, cty~displ + cyl)


# mod3 ####
mod3 <- glm(data = mpg, cty~displ * cyl) # interaction variable

# mod4 ####
mpg <- mpg %>%
    mutate(auto = grepl("auto", trans))

mod4 <- glm(data = mpg, cty~displ * cyl * auto)
summary(mod4)

mpg %>%
    ggplot(aes(x = displ,
               y = cty,
               color = factor(cyl))) +
    geom_smooth(method = "glm")

# compare models ####
compare_models(mod1, mod2, mod3, mod4)
compare_performance(mod1, mod2, mod3, mod4, mod_best)
# R2 - higher better, improvement of explanation over the mean
# RMSE - smaller better, how far is reality from prediction
# AIC - smaller better, how complex model is

compare_performance(mod1, mod2, mod3, mod4, mod_best) %>% plot()
# model = simplify -> predict

# mod 5 ####
mod5 <- glm(data = mpg, cty ~ displ * year + cyl *trans *drv * fl + class)

# MASS ####
step <- stepAIC(mod5)
step$model
step$formula

mod_best <- glm(data = mpg, formula = formula(step))
mod_best$formula


# predicting ####
mpg$pred1 <- predict(mod1, mpg)
mpg %>%
    ggplot(aes(x = cty, y = pred1)) +
    geom_point() # actual vs prediction
# if perfect straight line, perfect = bad

mpg$pred <- predict(mod, mpg)
mpg %>%
    ggplot(aes(x = cty, y = pred)) +
    geom_point()

# mod 2
mpg$pred2 <- predict(mod2, mpg)
mpg %>%
    ggplot(aes(x = cty, y = pred2)) +
    geom_point()

# mod 3
mpg$pred3 <- predict(mod3, mpg)
mpg %>%
    ggplot(aes(x = cty, y = pred3)) +
    geom_point()

# mod 4
mpg$pred4 <- predict(mod4, mpg)
mpg %>%
    ggplot(aes(x = cty, y = pred4)) +
    geom_point()

mpg$pred_best <- predict(mod_best, mpg)
mpg %>%
    ggplot(aes(x = cty, y = pred_best)) +
    geom_point()

mpg %>%
    pivot_longer(starts_with("pred")) %>%
    ggplot(aes(x = displ, y = cty, color = factor(cyl))) +
    geom_point() +
    geom_point(aes(y=value), color = 'black') +
    #geom_smooth(method = "lm", se=FALSE,color='red')+
    facet_wrap(~name)


mpg$displ %>%  range()
predict(mod1, data.frame(displ = 1:100)) %>% plot()

check_model(mod_best)

