library(psych)
library(leaps)
library(relaimpo)
library(gvlma)
# scientific notation
options(scipen=999)

advertisingData <- read.csv("C:/Users/lj015625/Desktop/ML Class/HW2/Advertising_data.csv")
summary(advertisingData)
advertisingModel <- lm(Sales~TV, data=advertisingData)
summary(advertisingModel)

advertisingFullModel <- lm(Sales~TV+Radio+Newspaper, data=advertisingData)
summary(advertisingFullModel)

cor(advertisingData)
