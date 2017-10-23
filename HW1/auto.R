library(psych)
AutoData <- read.csv("C:/Users/lj015625/Desktop/ML Class/HW1/auto_data.csv")
attach(AutoData)
summary(AutoData)
LinearRegModel <- lm(mpg~horsepower)
LinearRegModel
summary(LinearRegModel)
fitted(LinearRegModel)
plot(horsepower, mpg)
abline(LinearRegModel)
LinearRegModel2 <- lm(mpg~cylinders+displacement+horsepower+weight)
LinearRegModel2
summary(LinearRegModel2)
cor(AutoData[c("mpg","cylinders","displacement")])