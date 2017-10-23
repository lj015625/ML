# scientific notation
options(scipen=999)
library(psych)
library(outliers)
library(mlogit)

creditDefault_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/HW4/creditdefault_data.csv")
attach(creditDefault_data)
dim(creditDefault_data)
describe(creditDefault_data)
summary(creditDefault_data)

FREQ_TABLE<-table(creditDefault_data$default)
PROB_TABLE<-prop.table(FREQ_TABLE)
PROB_TABLE

percentage <- length(which(creditDefault_data$default=="Yes")) / nrow(creditDefault_data)
percentage

logitModelBalance <- glm(default~balance, family=binomial(), data=creditDefault_data)
summary(logitModelBalance)

contrasts(as.factor(creditDefault_data$default))

logitModelStudent <- glm(default~student=='No', family=binomial(), data=creditDefault_data)
summary(logitModelStudent)
