sharpe_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/sharpe_data.csv")
attach(sharpe_data)
dim(sharpe_data)
sharpeLinearModel <- lm(return~risk)
sharpeLinearModel
summary(sharpeLinearModel)
anova(sharpeLinearModel)
cor(return,risk)
