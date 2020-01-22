#Loading the required libraries
library('caret')
#Seeting the random seed
set.seed(1)

#Loading the hackathon dataset
data<-read.csv(url('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv'))

#Let's see if the structure of dataset data
str(data)

#Does the data contain missing values
sum(is.na(data))

#Imputing missing values using median
preProcValues <- preProcess(data, method = c("medianImpute","center","scale"))
library('RANN')
data_processed <- predict(preProcValues, data)

sum(is.na(data_processed))

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(data_processed$Loan_Status, p=0.75, list=FALSE)
trainSet1 <- data_processed[ index,]
testSet1 <- data_processed[-index,]
#Defining the training controls for multiple models
fitControl1 <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)
#Defining the predictors and outcome
predictors <- c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome", "CoapplicantIncome")
outcomeName <- 'Loan_Status'

#Training the random forest model
model_rf1<-train(trainSet1[,predictors],trainSet1[,outcomeName],method='rf',trControl=fitControl1,tuneLength=5)
model_rf1

#Predicting using random forest model
testSet1$pred_rf<-predict(object = model_rf1,testSet1[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet1$Loan_Status,testSet1$pred_rf)


#Training the Logistic regression model
model_lr1<-train(trainSet1[,predictors],trainSet1[,outcomeName],method='glm',trControl=fitControl, tuneLength=1)

#Predicting using knn model
testSet1$pred_lr<-predict(object = model_lr1,testSet1[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet1$Loan_Status,testSet1$pred_lr)

