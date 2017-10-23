# scientific notation
options(scipen=999)
library(psych)
library(outliers)
library(mlogit)
library(ggplot2)

admission_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/data/admission_data.csv")
attach(admission_data)
dim(admission_data)
describe(admission_data)

logitModel <- glm(admission~gpa+gre+rank, family=binomial(), data=admission_data)
summary(logitModel)

logits <- predict(logitModel)
odds <- exp(logits)
probability <- data.frame(odds/(1+odds))
colnames(probability) <- c("probability")
# the mean should be close to 50%
describe(probability)

predicted_admission <- data.frame(admission_data, probability)


ggplot(predicted_admission, aes(x = gpa, y = probability)) +
  geom_point(color = "red", shape=15, size = 1) +
  ggtitle("gpa vs. probability")