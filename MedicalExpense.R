dat2 <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/medexpense_data.csv")
attach(dat2)
names(dat2)
dim(dat2)
summary(dat2)
describe(dat2)
library(psych)
plot(age, medical_expenses)
cor(age,medical_expenses)
hist(medical_expenses)
cor(dat2[c("medical_expenses","age","bmi")])
table(smoker)
table(gender)

twoWayTable <- table(smoker,gender)
twoWayTable

regModel <- lm(medical_expenses~age+bmi+children+smoker+gender)
regModel
anova(regModel)
summary(regModel)
contrasts(as.factor(smoker))
contrasts(as.factor(gender))
