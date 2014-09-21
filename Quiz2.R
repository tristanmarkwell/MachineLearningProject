## Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p=.5, list=FALSE)
training = adData[-testIndex,]
testing=adData[testIndex,]
## Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(y=CompressiveStrength, data=training, col=Age)
## Question 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(Superplasticizer, data=training)
summary(training$Superplasticizer)
qplot(log(Superplasticizer+1), data=training)
## Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
trainingIL <- training[,c("IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3",
                          "IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8")]
preProc <- preProcess(trainingIL, method="pca",thresh=.9)
preProc
## Question 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
testing = adData[-inTrain,]
trainingIL <- training[,c("IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3",
                          "IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8")]
testingIL <- testing[,c("IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3",
                 "IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8")]
preProc <- preProcess(trainingIL, method="pca",thresh=.8)
trainingPCA <- predict(preProc, trainingIL)
testingPCA <- predict(preProc, testingIL)
model1 <- train(trainingIL, training$diagnosis, method="glm")
model2 <- train(trainingPCA, training$diagnosis, method="glm")
predict1 <- predict(model1, newdata=testingIL)
predict2 <- predict(model2, newdata=testingPCA)
mean(predict1==testing$diagnosis)
mean(predict2==testing$diagnosis)
confusionMatrix(testing$diagnosis, predict1)
confusionMatrix(testing$diagnosis, predict2)
