library(psych)
library(leaps)
library(relaimpo)
library(gvlma)
# scientific notation
options(scipen=999)

##################BETA REGRESSIONS###############################################
murder_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/murder_data.csv")

attach(murder_data)
dim(murder_data)
describe(murder_data)
cor(murder_data)

murder_model <- lm(murder_rate~population+illiteracy+income+frost, data=murder_data)
summary(murder_model)
anova(murder_model)
gvlma(murder_model)
confint (murder_model)

scaled_data <- scale(murder_data)
describe(scaled_data)

z_data <- data.frame(scaled_data)

z_model <- lm(murder_rate~population+illiteracy+income+frost, data=z_data)
summary(z_model)


# Regression subsets
reg_subsets <- regsubsets(murder_rate~population+illiteracy+income+frost, data=z_data, nvmax=4)
summary(reg_subsets)

summary_of_regressions<-summary(reg_subsets)
names(summary_of_regressions)

summary_of_regressions$adjr2

highest_rsqure_model <- lm(murder_rate~population+illiteracy, data=murder_data)
summary(highest_rsqure_model)


# HW
reg_subsets <- regsubsets(murder_rate~., data=z_data, nvmax=7)
summary(reg_subsets)
summary_of_regressions<-summary(reg_subsets)
summary_of_regressions$adjr2

highest_rsqure_model <- lm(murder_rate~population+illiteracy+frost+life_expectancy+area, data=murder_data)
summary(highest_rsqure_model)

scaled_data <- scale(murder_data)
describe(scaled_data)

z_data <- data.frame(scaled_data)

z_model <- lm(murder_rate~population+illiteracy+frost+life_expectancy+area, data=z_data)
summary(z_model)
calc.relimp(z_model, type=c("lmg", "last", "first", "pratt"), rela=TRUE)
