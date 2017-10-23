library(psych)
library(leaps)
library(relaimpo)
library(gvlma)
# scientific notation
options(scipen=999)

mutal_fund <- read.csv("C:/Users/lj015625/Desktop/ML Class/Data/mutual_fund_data.csv")

attach(mutal_fund)
dim(mutal_fund)
describe(mutal_fund)

mutualModel <- lm(return~nav+expense_ratio+fund_type, data=mutal_fund)
summary(mutualModel)
predict(mutualModel)
describe(predict(mutualModel))
anova(mutualModel)
contrasts(as.factor(mutal_fund$fund_type))
cor(mutal_fund)

internationalFund <- mutal_fund[which(mutal_fund$fund_type=="I"),]
describe(internationalFund)
domesticFund <- mutal_fund[which(mutal_fund$fund_type=="D"),]
describe(domesticFund)

predictedInternationFund <- predict(mutualModel, data.frame(internationalFund))
describe(predictedInternationFund)

predictedDomesticFund <- predict(mutualModel, data.frame(domesticFund))
describe(predictedDomesticFund)

##################BETA REGRESSIONS###############################################
mutal_fund$fund_type=NULL
mutal_fund$mutual_fund= NULL

scaled_data <- scale(mutal_fund)
describe(scaled_data)

z_data <- data.frame(scaled_data)

mutualModel2 <- lm(return~nav+expense_ratio, data=z_data)
summary(mutualModel2)
describe(predict(mutualModel2))


