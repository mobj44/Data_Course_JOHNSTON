library(pdftools)
library(tidyverse)
library(caret)
library(broom)
library(modelr)
library(kableExtra)
library(easystats)
library(vegan) #vegitation analysis


mod1 <- mpg %>%
    glm(data = .,
        formula = cty ~ displ + drv) # train/fit model
broom::tidy(mod1) %>%  # turns model output into a data frame
    kableExtra::kable() %>%
    kable_classic(lightable_options = 'hover')

add_predictions(mpg,mod1) %>% # can give any data set with same columns as original
    ggplot(aes(x=pred, y=cty)) +
    geom_point()

add_residuals(mpg,mod1) %>% # how far off our predictions are
    ggplot(aes(x=resid, y=cty)) +
    geom_point()

# cross validataion (does not improve your model, just tests how well it does)
# test model on new data (new data has to have actual answers)
# set aside random subset
# need to subset from every factor

mpg$drv %>% table

id <- caret::createDataPartition(mpg$cty, p = 0.8, list = FALSE) # data partition
train <- mpg[id,]
test <- mpg[-id,]

# train model on data set
mod2 <- train %>%
    glm(data = .,
        formula = cty ~ displ + drv)
broom::tidy(mod2) %>%
    kableExtra::kable() %>%
    kable_classic(lightable_options = 'hover')

add_predictions(test,mod2) %>%
    mutate(error = abs(pred - cty)) %>%
    pluck("error")  %>%
    summary()

add_predictions(mpg,mod1) %>%
    mutate(error = abs(pred - cty)) %>%
    pluck("error")  %>%
    summary()


# clustering
names(iris)

iris %>%
    ggplot(aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
    geom_point() +
    stat_ellipse()

mat <- iris %>%
    select(-Species) %>%
    as.matrix()
adonis2(mat ~ iris$Species) # permutational analysis of variance (ANOVA)
# multidemensional scaling - compressed into 2 dimensions
mds <- metaMDS(mat)
data.frame(species = iris$Species,
           mds1 = mds$points[,1],
           mds2 = mds$points[,2]) %>%
    ggplot(aes(x=mds1, y = mds2, color = species)) +
    geom_point() +
    stat_ellipse()

kmeans() # tidyclust package

