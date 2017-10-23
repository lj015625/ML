library(psych)
library(leaps)
library(relaimpo)
library(gvlma)
# scientific notation
options(scipen=999)

bostonData <- read.csv("C:/Users/lj015625/Desktop/ML Class/HW2/boston_data.csv")
attach(bostonData)
dim(bostonData)
summary(bostonData)
describe(bostonData)
cor(bostonData)

bostonRegModel <- lm(crim~medv, data=bostonData)
summary(bostonRegModel)
anova(bostonRegModel)

totalSumOfSquares = sum((bostonData$crim - mean(bostonData$crim))^2) 
totalSumOfSquares
residualSumOfSquares = sum(bostonRegModel$residuals^2)
residualSumOfSquares
regressionSumOfSquares = sum((bostonRegModel$fitted.values - mean(bostonData$crim))^2)
regressionSumOfSquares
R2 = regressionSumOfSquares/totalSumOfSquares
R2

bostonRegFullModel <- lm(crim~.-X, data=bostonData)
summary(bostonRegFullModel)
anova(bostonRegFullModel)

bostonReducedModel <- lm(crim~zn+dis+rad+black+medv)
summary(bostonReducedModel)

bostonData[500,]
predict(bostonRegFullModel)[500]
resid(bostonRegFullModel)[500]
#bostonRegFullModel$residuals[500]



