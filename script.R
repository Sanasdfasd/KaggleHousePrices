#In this simple model I will use random forests on the same few selected features as in Simplemodel
library(randomForest)

#Read the train file
tr <- read.csv("../input/train.csv")
te <- read.csv("../input/test.csv")

Bath <- pmax(1,tr$FullBath)
Bed <- pmin(pmax(1, tr$BedroomAbvGr), 4)
LogLotArea <- log10(tr$LotArea)
LogArea <- log10(tr$GrLivArea)
Age <- pmax(0.0, tr$YrSold - pmax(tr$YearBuilt, tr$YearRemodAdd))
New <- as.factor(Age == 0.0)
Quality <- tr$OverallQual
Neighborhood <- as.factor(tr$Neighborhood)
LogPrice <- log10(tr$SalePrice)
fewfeatures.train <- data.frame(Bath, Bed, LogArea, LogLotArea, New, Age, Quality, Neighborhood)

simplemodel <- randomForest(LogPrice ~ ., data = fewfeatures.train)

simplemodel
summary(simplemodel)

Bath <- pmax(1,te$FullBath)
Bed <- pmin(pmax(1, te$BedroomAbvGr), 4)
LogArea <- log10(te$GrLivArea)
LogLotArea <- log10(te$LotArea)
Age <- pmax(0.0, te$YrSold - pmax(te$YearBuilt, te$YearRemodAdd))
New <- as.factor(Age == 0.0)
Quality <- te$OverallQual
Neighborhood <- as.factor(te$Neighborhood)

fewfeatures.test <- data.frame(Bath, Bed, LogArea, LogLotArea, New, Age, Quality, Neighborhood)

test.LogPrice <- predict(simplemodel, fewfeatures.test)
SalePrice <- 10.0**test.LogPrice
Id <- te$Id

submission <- data.frame(Id, SalePrice)

write.csv(submission, "submission.csv", row.names = F)






