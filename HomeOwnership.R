# scientific notation
options(scipen=999)
library(psych)
library(outliers)
library(mlogit)

ownership_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/data/home_ownership_data.csv")
attach(ownership_data)
names(ownership_data)
str(ownership_data)
describe(ownership_data)
outlier(income, data=ownership_data)
outlier(ownership_data)

LPM<-lm(home_ownership~income)
summary(LPM)

predicted_value <- data.frame(predict(LPM))
