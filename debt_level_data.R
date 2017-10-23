# scientific notation
options(scipen=999)
library(psych)
library(outliers)
library(mlogit)

debt_level_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/data/debt_level_data.csv")
attach(debt_level_data)
dim(debt_level_data)
describe(debt_level_data)
debt_level_data[44,]
debt_level_data[25,]

logitModel <- glm(debt_level~annual_income+household_size, family=binomial(), data=debt_level_data)
summary(logitModel)

logits <- data.frame(predict(logitModel))
odds <- data.frame(exp(logits))
probability <- data.frame(odds/(1+odds))
describe(probability)

logitModel$fitted.values
