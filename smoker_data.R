# scientific notation
options(scipen=999)
library(psych)
library(outliers)
library(mlogit)

smokerData <- read.csv("C:/Users/lj015625/Desktop/ML Class/data/smokers_data.csv")
attach(smokerData)
dim(smokerData)
describe(smokerData)

FREQ_TABLE<-table(smokerData$smoker)
PROB_TABLE<-prop.table(FREQ_TABLE)
PROB_TABLE

logitModel <- glm(smoker~age+education+income+price_cigs, family=binomial(), data=smokerData)
summary(logitModel)


logits <- data.frame(predict(logitModel))
odds <- data.frame(exp(logits))
probability <- data.frame(odds/(1+odds))
describe(probability)
