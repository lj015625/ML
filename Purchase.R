# scientific notation
options(scipen=999)
library(psych)
library(outliers)
library(mlogit)

purchase_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/data/purchase_data.csv")
attach(purchase_data)
dim(purchase_data)
describe(purchase_data)

logitModel <- glm(purchase~income+age+zip, family=binomial(), data=purchase_data)
summary(logitModel)
logitModel$fitted.values;

logits <- predict(logitModel)
odds <- exp(logits)
probability <- odds/(1+odds)

# the mean should be close to 50%
describe(probability)
