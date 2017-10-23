library(psych)
library(leaps)
library(relaimpo)
library(gvlma)
# scientific notation
options(scipen=999)

##################BETA REGRESSIONS###############################################
medexpenses_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/medexpenses_data.csv")

attach(medexpenses_data)
dim(medexpenses_data)
describe(medexpenses_data)

reg_model <- lm(medical_expenses~age+bmi)
summary(reg_model)
anova(reg_model)
gvlma(reg_model)

scaled_data <- scale(medexpenses_data)
scaled_data

describe(scaled_data)

z_data <- data.frame(scaled_data)

z_model <- lm(medical_expenses~age+bmi, data=z_data)
summary(z_model)


calc.relimp(z_model, type=c("lmg", "last", "first", "pratt"), rela=TRUE)



