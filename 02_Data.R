# Exercise 1
land = c("Belgium","Denmark","France", "GB","Ireland", "Italy", "Luxembourg", "Holland",
         "Portugal", "Spain","USA", "Japan", "Deutschland")
x = c(2.8, 1.2, 2.1, 1.6, 1.5, 4.6,  3.6, 2.1, 6.5, 4.6, 3.0, 1.3, 4.2)
y = c(9.4,10.4,10.8, 10.5,18.4,11.1,2.6,8.8, 5.0, 21.5,6.7, 2.5,5.6)
df = data.frame(land,x,y)

# a
print(paste(df$land[[which.max(df$x)]][1], max(df$x)))
# I feel this is not the best way to do it

x.max = df[which.max(df$x), ][,c("land", "x")]
y.max = df[which.max(df$y), ][,c("land", "y")]
x.min = df[which.min(df$x), ][,c("land", "x")]
y.min = df[which.min(df$y), ][,c("land", "y")]
# this already looks better

# b
x.range = x.max$x - x.min$x
y.range = y.max$y - y.min$y


# Exercise 2
library(datasets)
data(mtcars)
help(mtcars)


# Exercise 3
mtcars = mtcars[order(mtcars$mpg , mtcars$cyl), ]

# Exercise 4
mtcars$carb = NULL

# Exercise 5
colnames(mtcars)
r.cars = subset(mtcars, select=c("hp",  "cyl",  "disp", "mpg",   "drat", "wt",   "qsec", "vs",   "am",   "gear"))

# Exercise 6
mtcars[grep("Merc", rownames(mtcars)), ]

# Exercise 7
dax.prices = read.csv("C:\\Users\\User\\Downloads\\dax_prices.csv")

# Exercise 8
is.numeric(dax.prices$DAX)

# Exercise 9
names(dax.prices)[names(dax.prices) == "DAX"] = "DAX Prices"

# Exercise 10
write.table(dax.prices, file = "dax.prices.txt" , sep = ";", dec = ",")

# Exercise 11
