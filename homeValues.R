options(sipen=999)
library(psych)

home_values_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/home_values_data.csv")
attach(home_values_data)

describe(home_values_data)
figure1 <- plot(size, home_price)
reg1 <- lm(home_price~size)
summary(reg1)
abline(reg1)

squared_term1 <- (size*size)
polynomial_reg1 <- lm(home_price~size+squared_term1)
summary(polynomial_reg1)


tortoise_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/tortoise_data.csv")
attach(tortoise_data)
describe((tortoise_data))
figure2 <- plot(shell_size, number_of_eggs)
reg2 <- lm(number_of_eggs~shell_size)
summary(reg2)

squared_term2 <- (shell_size^2)
reg3 <- lm(number_of_eggs~shell_size+squared_term2)
summary(reg3)

tortoise_data2 <- data.frame(shell_size=c(350))
predict(reg3, newData=tortoise_data2)
