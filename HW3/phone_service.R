# scientific notation
options(scipen=999)
library(psych)
library(ggplot2)

phone_service_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/HW3/phone_service_data.csv")
attach(phone_service_data)
summary(phone_service_data)
describe(phone_service_data)


ggplot(phone_service_data, aes(x = customers, y = line_maintenance_.expense)) +
  geom_point(color = "forestgreen", shape=15, size = 3) +
  ggtitle("Customer vs. Expense")


reg1 <- lm(line_maintenance_.expense~customers, data=phone_service_data)
summary(reg1)
anova(reg1)

totalSumOfSquares = sum((phone_service_data$line_maintenance_.expense - mean(phone_service_data$line_maintenance_.expense))^2) 
residualSumOfSquares = sum(reg1$residuals^2)
regressionSumOfSquares = sum((reg1$fitted.values - mean(phone_service_data$line_maintenance_.expense))^2)
R2 = regressionSumOfSquares/totalSumOfSquares
R2

predict(reg1)
plot(customers, line_maintenance_.expense)
abline(reg1)

customerSquareTerm <- customers^2
reg2 <- lm(line_maintenance_.expense~customers+customerSquareTerm, data=phone_service_data)
summary(reg2)
anova(reg2)

predict(reg2)