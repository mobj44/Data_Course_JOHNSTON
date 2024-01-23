cars <- mtcars[mtcars$cyl > 4,]

miles <- cars$mpg

min(miles)
mean(miles)
max(miles)

# ending comment with 4 hashtags creates outline
# object types ####
# 1 dim
## logical ####
c(TRUE, TRUE, FALSE, TRUE)
## numeric ####
1:10
## character ####
letters[3]
## integer ####
c(1L,2L,3L)
# 2 dims
##data frame
mtcars[rows,cols]
## factor ####
# for categorical data, stored as numbers, has levels
# if it says levels, always factor
as.factor(letters)

haircolors <- c("brown","black","red","blonde", "red", "black")
haircolor_factor <- as.factor(haircolors)
levels(haircolor_factor)


# type conversions ####
1:5 # numeric
as.character(1:5) # character
as.numeric(letters)
as.numeric(c("1", "b", "35"))
## logicals ####
x <- as.logical(c("true","t","F","False", "T"))
sum(TRUE)
TRUE + TRUE
FALSE + 3

sum(x,na.rm = TRUE) # important

# data frames ####
str(mtcars) # structure
names(mtcars) # character vector of column names

as.character(mtcars)

as.character(mtcars$mpg)

mtcars[,"mpg"]

# loop assigns character version of every column over itself
for (i in names(mtcars)) {
    mtcars[,i] <- as.character(mtcars[,i])
}

# shorter way to do above for loop
# "apply" family functions
apply(mtcars,2,as.character) # 2 = margain(rows-1,col-2)
# array, margains, function
# different versions of apply for different data types
# ex. lapply()


str(mtcars)
data("mtcars") # resets built in data frame

path <- "./Data/cleaned_bird_data.csv"
df <- read.csv(path, header = TRUE)
str(df)

for (i in names(df)) {
    df[,i] <- as.character(df[,i])
}
str(df)
write.csv(df,"./Data/new_bird_file")

# Packages ####

## tidyverse ####
library(tidyverse)

# %>% is a pipe
mtcars %>%
    filter(mpg > 19 & vs == 1)

mtcars$mpg %>%
    mean() %>%
    round(digits = 2)


#diff versions same func
stats::filter()
dplyr::filter()

