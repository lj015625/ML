insurance_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/insurance_data.csv")
attach(insurance_data)

library(psych)
describe(insurance_data)
str(insurance_data)
reg1 <- lm(medical_expenses~age+bmi+smoker, data=insurance_data)
summary(reg1)

obesity <- (ifelse(bmi >= 30, 1, 0))

obesity_smoker <- (obesity*smoker)

reg2 <- lm(medical_expenses~age+bmi+smoker+obesity_smoker, data=insurance_data)
summary(reg2)

fitted_values <- data.frame(predict(reg2))
