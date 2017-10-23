library(psych)
library(leaps)
library(relaimpo)
library(gvlma)
library(ggplot2)
library(ggfortify)

# scientific notation
options(scipen=999)

housing_data <- read.csv("C:/Users/lj015625/Desktop/ML Class/Final/train.csv")
attach(housing_data)
dim(housing_data)
describe(housing_data)
summary(housing_data)
summary(housing_data$SalePrice)
summary(housing_data$YrSold)
summary(housing_data[housing_data$Neighborhood=='NridgHt',]$SalePrice)
summary(housing_data[housing_data$Neighborhood=='NPkVill',]$SalePrice)

cor(housing_data$SalePrice, housing_data$OverallQual)
cor(housing_data$SalePrice, housing_data$GrLivArea)

subsetData = data.frame(housing_data$SalePrice, housing_data$OverallQual, housing_data$LotArea, housing_data$GrLivArea, housing_data$GarageCars, 
                        housing_data$BedroomAbvGr, housing_data$KitchenAbvGr, 
                        housing_data$TotalBsmtSF, housing_data$YearBuilt)
cor(subsetData)


# trainingRatio = .8
# housing_data[,"train"] <- ifelse(runif(nrow(housing_data)) < trainingRatio, 1, 0)
# #get col number of train / test indicator column
# trainColNum <- grep('train', names(housing_data))
# #separate training and test sets and remove training column before modeling
# trainData <- housing_data[housing_data$train==1, -trainColNum]
# testData <- housing_data[housing_data$train==0, -trainColNum]


# visualization by ggplot
ggplot(housing_data, aes(x = OverallQual, y = SalePrice)) +
  geom_point(color = "forestgreen", shape=2, size = 1) + theme_bw() + xlab("Overall quality from 1 to 10 scale") + ylab("Sales Price in USD") +
  ggtitle("Overall Quality Vs. Sales Price") + stat_smooth(method = "lm", col = "red", se=FALSE)

ggplot(housing_data, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(color = "blue", shape=4, size = 1) + theme_bw() + xlab("Living Area in Square Feet") + ylab("Sales Price in USD") +
  ggtitle("Living Area Vs. Sales Price") + stat_smooth(method = "lm", col = "red", se=FALSE)

ggplot(housing_data, aes(fill = Neighborhood, factor(Neighborhood), SalePrice)) + 
  geom_boxplot() + theme_bw() + xlab("Neighborhood name") + ylab("Sales Price in USD") +
  ggtitle("Neighborhood Vs. Sales Price") 



# Three variables overall quality and neighborhood

regModel <- lm(SalePrice~OverallQual+Neighborhood+GrLivArea, data=housing_data)
summary(regModel)
anova(regModel)
autoplot(regModel4, label.size = 3) + theme_bw()

# residuals
autoplot(regModel, label.size = 3) + theme_bw()

#Beta regression
subsetData = data.frame(housing_data$SalePrice, housing_data$OverallQual, housing_data$LotArea, housing_data$GrLivArea, housing_data$GarageArea, 
                        housing_data$BedroomAbvGr, housing_data$TotRmsAbvGrd, housing_data$KitchenAbvGr, housing_data$FullBath, housing_data$HalfBath, 
                        housing_data$TotalBsmtSF, housing_data$YearBuilt, housing_data$YearRemodAdd)
summary(subsetData)


scaled_data <- scale(subsetData)
describe(scaled_data)
z_data <- data.frame(scaled_data)
reg_subsets <- regsubsets(SalePrice~OverallQual+LotArea+GrLivArea+GarageArea+BedroomAbvGr+TotRmsAbvGrd+KitchenAbvGr+FullBath+HalfBath+TotalBsmtSF
                          +YearBuilt+YearRemodAdd, data=z_data, nvmax=12)
summary(reg_subsets)
summary_of_regressions<-data.frame(summary(reg_subsets))
summary_of_regressions$adjr2


# add 10 quantitaive variables
regMode2 <- lm(SalePrice~OverallQual+Neighborhood+LotArea+GrLivArea+GarageArea+BedroomAbvGr+TotRmsAbvGrd+KitchenAbvGr+FullBath+HalfBath+TotalBsmtSF)
summary(regMode2)
anova(regMode2)
autoplot(regModel2, label.size = 3) + theme_bw()

# add 17 categorial and quantitaive variables
regMode3 <- lm(SalePrice~OverallQual+Neighborhood+LotArea+GrLivArea+GarageCars+MasVnrArea+GarageArea+BedroomAbvGr+KitchenAbvGr+TotalBsmtSF+YearBuilt+
                 YearRemodAdd+SaleType+SaleCondition+HouseStyle+RoofMatl+MSZoning+BldgType+Foundation)
summary(regMode3)
anova(regMode3)
autoplot(regModel3, label.size = 3) + theme_bw()

# interaction term Neighborhood*OverallQual
largeHome <- (ifelse(GrLivArea >= 3000, 1, 0))
largeGoodQualityHome <- (largeHome*OverallQual)
regModel4 <- lm(SalePrice~largeGoodQualityHome+Neighborhood+OverallQual+LotArea+GrLivArea+MasVnrArea+GarageArea+BedroomAbvGr+KitchenAbvGr+TotalBsmtSF+YearBuilt+
                  YearRemodAdd+SaleType+SaleCondition+HouseStyle+RoofMatl+MSZoning+BldgType+Foundation)
summary(regModel4)
anova(regModel4)
autoplot(regModel4, label.size = 3) + theme_bw()

# multiple interaction terms
regModel5 <- lm(SalePrice~largeGoodQualityHome+Neighborhood*OverallQual+Neighborhood*BldgType+KitchenAbvGr*KitchenQual+RoofStyle*RoofMatl+
                  MasVnrType*MasVnrArea+LotArea*LotShape+BsmtUnfSF*TotalBsmtSF+
                  SaleType*SaleCondition+YearBuilt*YearRemodAdd+BsmtQual*BsmtCond+
                  +GrLivArea+GarageCars+BedroomAbvGr+MSZoning+Foundation)
summary(regModel5)
anova(regModel5)
autoplot(regModel5, label.size = 3) + theme_bw()

# reduce overfitting 
reducedModel <- lm(SalePrice~largeGoodQualityHome+Neighborhood*OverallQual+BldgType+KitchenAbvGr+KitchenQual+RoofStyle+RoofMatl+
                  LotArea+BsmtUnfSF+TotalBsmtSF+SaleType+SaleCondition+YearBuilt+YearRemodAdd)
summary(reducedModel)
anova(reducedModel)
confint (reducedModel)

autoplot(reducedModel, label.size = 3) + theme_bw()


