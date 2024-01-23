x <- c(1,2,3,4,5)
class(x) # numeric
class(class(x)) #character -- its in a string

length(x) # 5
length(length(x)) # 1


for (i in x) {
    print(1+i)a
}
x^3
1:5
seq(1,1000,by=2)

z <- c('a','2')

letters <- c('a','b','c','d','e','f','g','h','i','j')
a <- 1:10
b <- 2:11
c <- letters

cbind(a,b,c)
d <- rep(TRUE,10)
class(d) # logical - basically bool

z <- data.frame(a,b,c,d)

# class length dimensions
class(z)
length(z)
dim(z)

# logical
1 > 0
0 >= 0
3 < 1
1 == 1
1 != 1
5 > a
a[5 > a]
z[5 > a,] #, indicates all dimensions of df

# data frame has 2 dimensions [row, column]
z[1,] # row 1 all columns
z[,1] # all rows no columns

# rows where c = b
z[c=='b',]


iris
# rows where sepal length > 5
iris$Sepal.Length > 5
big_iris <- iris[iris$Sepal.Length > 5,]

dim(big_iris) # dim = row, column
nrow(big_iris) # number of rows

# adds new column to big_iris
big_iris$Sepal.Area <- big_iris$Sepal.Length*big_iris$Sepal.Width

# setosa from big iris
big_setosa <- big_iris[big_iris$Species == "setosa",]
big_setosa

# mean sepal area from big_setosa
mean(big_setosa$Sepal.Area)
plot(big_setosa$Sepal.Length, big_setosa$Sepal.Width)
sd(big_setosa$Sepal.Area)
summary(big_setosa$Sepal.Area)

# spend several hours playing with vectors
# give me rows of iris where species setosa or virginica and area > 5

# how to make qr codes
install.packages('qrcode')

url <- "https://github.com/mobj44"
qr <- qrcode::qr_code(url)
plot(qr)


iris %>%
    filter(Sepal.Length > 5) %>%
    mutate(Sepal.Area = Sepal.Length*Sepal.Width) %>%
    ggplot(aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
    geom_point()
