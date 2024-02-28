library(tidyverse)
library('pROC')

titanicTrain = read.csv("~/Desktop/TitanicData.csv")

sapply(titanicTrain,class)

titanicTrain$Survived = as.factor(titanicTrain$Survived)
titanicTrain$Pclass = as.factor(titanicTrain$Pclass)
titanicTrain$Sex = as.factor(titanicTrain$Sex)
titanicTrain$Cabin = as.factor(titanicTrain$Cabin)
titanicTrain$Embarked = as.factor(titanicTrain$Embarked)

ggplot(data = titanicTrain,aes(x=Embarked,fill=Survived))+geom_bar()+labs(y ="Frequency", x = "Embarked") + theme_bw()


titanicTrain$Age[is.na(titanicTrain$Age)] = mean(titanicTrain$Age, na.rm = TRUE) 

titanicTrain = titanicTrain %>% filter(Embarked != '') 

titanicTrain = titanicTrain %>% mutate(SurvivedEncoded = ifelse(Survived == 1, "Yes", "No")) 

titanicTrain$SurvivedEncoded = as.factor(titanicTrain$SurvivedEncoded)

fit_logistic = glm(SurvivedEncoded~ Pclass+ Sex + Age + SibSp, data = titanicTrain, family = "binomial")

summary(fit_logistic)

testData = read.csv("~/Desktop/DataForPredictionTitanic.csv")

sapply(testData, class)

testData$Pclass = as.factor(testData$Pclass)
testData$Sex = as.factor(testData$Sex)
testData$Age = as.numeric(testData$Age)
testData$Survived = as.factor(testData$Survived)

predictedData = predict(fit_logistic, testData, type = "response")
head(predictedData)

predictedValDataSet = cbind(testData, PredictedData = predictedData)

predictedValDataSet = predictedValDataSet %>% mutate(PredictedSurvival = ifelse(PredictedData > 0.5, "Yes", "No"))

with(predictedValDataSet, table(PredictedSurvival, Survived))

auc(response = predictedValDataSet$Survived, predictor = predictedValDataSet$PredictedData)

fit_logistic2 = glm(SurvivedEncoded~ Sex + Age, data = titanicTrain, family ="binomial")
summary(fit_logistic2)

predictedProb = predict(fit_logistic2, testData, type = "response")
predictedValDataSet = cbind(predictedValDataSet, PredictedProbMod2 = predictedProb)
predictedValDataSet = predictedValDataSet %>% mutate(PredictedClassMod2 =ifelse(PredictedProbMod2 > 0.5, "Yes", "No"))

with(predictedValDataSet,table(PredictedClassMod2, Survived))

auc(response = predictedValDataSet$Survived, predictor = predictedValDataSet$PredictedProbMod2)
