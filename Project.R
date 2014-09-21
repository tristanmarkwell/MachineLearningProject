##Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. 
##Wearable Computing: Accelerometers' Data Classification of Body Postures and 
##Movements. Proceedings of 21st Brazilian Symposium on Artificial Intelligence. 
##Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in Computer 
##Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. ISBN 
##978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6_6. 
##Read more: http://groupware.les.inf.puc-rio.br/har#ixzz3Dv2Iq2vD
set.seed(1234)
library(caret)
library(gbm)
if (!file.exists('data')) dir.create('data')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',
              '.\\data\\training.csv')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',
              '.\\data\\testing.csv')
downloadDate <- date()
trainingData <- read.csv('.\\data\\training.csv')
testing <- read.csv('.\\data\\testing.csv')
partition <- createDataPartition(y=trainingData$classe, p=.7, list=FALSE)
training <- trainingData[partition, ]
finalTraining <- training[,colMeans(is.na(training))<.5 & colMeans(is.na(testing))<.5]
##most of the factors are actually numbers - force numeric
finalTraining$X <- NULL
finalTraining$user_name <- NULL ## bring back for prediction
finalTraining$cvtd_timestamp <- NULL
finalTraining$classe <- NULL
finalTraining <- as.data.frame(lapply(finalTraining,as.numeric))
finalTraining$user_name <- training$user_name
finalTraining$classe <- training$classe
dummies <- dummyVars(classe ~ ., data=finalTraining)
finalTraining <- data.frame(predict(dummies, newdata = finalTraining))
finalTraining$classe <- training$classe
fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 2,
  ## repeated five times
  repeats = 2)
##modFit <- gbm(classe~., data=finalTraining)
modFit <- train(classe~., method="multinom", trControl=fitControl, data=finalTraining)
## processing validation set just like training
validation <- trainingData[-partition, ]
finalValidation <- validation[,colMeans(is.na(training))<.5 & colMeans(is.na(testing))<.5]
##most of the factors are actually numbers - force numeric
finalValidation$X <- NULL
finalValidation$user_name <- NULL ## bring back for prediction
finalValidation$cvtd_timestamp <- NULL
finalValidation$classe <- NULL
finalValidation <- as.data.frame(lapply(finalValidation,as.numeric))
finalValidation$user_name <- validation$user_name
finalValidation$classe <- validation$classe
finalValidation <- data.frame(predict(dummies, newdata = finalValidation))
validationPreds <- data.frame(predict(modFit, newdata=finalValidation))
##validationPreds <- apply(validationPreds,1,which.max)
##validationPreds <- as.factor(validationPreds)
##levels(validationPreds) <- c("A","B","C","D","E")
mean(validation$classe==validationPreds[,1])
## now for the test data
finalTesting <- testing[,colMeans(is.na(training))<.5 & colMeans(is.na(testing))<.5]
##most of the factors are actually numbers - force numeric
finalTesting$X <- NULL
finalTesting$problem_id <- NULL
finalTesting$user_name <- NULL ## bring back for prediction
finalTesting$cvtd_timestamp <- NULL
finalTesting$classe <- NULL
finalTesting <- as.data.frame(lapply(finalTesting,as.numeric))
finalTesting$user_name <- testing$user_name
finalTesting$classe <- rep("A",20)
finalTesting <- data.frame(predict(dummies, newdata = finalTesting))
finalTesting$classe <- rep("A",20)
testingPreds <- predict(modFit, newdata=finalTesting)
testingPreds

  n = length(testingPreds)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(testingPreds[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
