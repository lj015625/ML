library(psych)
CarSeats <- read.csv("C:/Users/lj015625/Desktop/ML Class/HW1/carseats_data.csv")
attach(CarSeats)
summary(CarSeats)
SalesOverPriceModel <- lm(Sales~Price)
SalesOverPriceModel
summary(SalesOverPriceModel)
fitted(SalesOverPriceModel)
plot(Price, Sales)
abline(SalesOverPriceModel)
SalesModel <- lm(Sales~Price+Competitor_Price+Income+Advertising)
SalesModel
summary(SalesModel)
anova(SalesModel)
