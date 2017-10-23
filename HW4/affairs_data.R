# scientific notation
options(scipen=999)
library(psych)
library(outliers)
library(mlogit)

affairs_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/HW4/affairs_data.csv")
attach(affairs_data)
dim(affairs_data)
describe(affairs_data)
PROBABILITY_TABLE<-prop.table(table(affairs_data$number_affairs))
PROBABILITY_TABLE
affairsTable <- data.frame(table(affairs_data$number_affairs))
frequencyTable <- data.frame(affairsTable, affairsTable$Freq/nrow(affairs_data))
names(frequencyTable) <- c("number_affairs","count","percentage")
frequencyTable

logitModel <- glm(YNAFFAIRS~male+age+years_married+children+religious+education+occupation+marriage_rating, 
                  family=binomial(), data=affairs_data)
summary(logitModel)

reducedLogitModel <- glm(YNAFFAIRS~age+years_married+religious+marriage_rating, 
                         family=binomial(), data=affairs_data)
summary(reducedLogitModel)

logits <- predict(reducedLogitModel)
odds <- exp(logits)
probability <- data.frame(odds/(1+odds))
colnames(probability) <- c("probability")
# the mean should be close to 50%
describe(probability)
