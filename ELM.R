#----------------Om NamahShivaaya-----------------#
#----------------ShivaayaNamah Om-----------------#

# Setting the path to add libraries 
setwd('/media/sharma/6E88CA0788C9CE31/Research/SixthSemester/ExperimentalWork/Programs/R/')
source("Libraries.R")

# Data collection: Merging all individual files (Which contain class level metrics), totalling to 34.
setwd('/media/sharma/6E88CA0788C9CE31/Research/SixthSemester/ExperimentalWork/DataSets/PROMISE/')

# Reading all file names from the current directory
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){read.csv(i,header=FALSE, skip=1)})
PROMISE <- do.call(rbind.data.frame, All)
str(PROMISE)
nrow(PROMISE)

# Here in this space, I've to write a code to read original names of the attributes and assign it to PROMISE dataset.

# Getting to know whether the module is defective or not
PROMISE$V21 <- as.numeric(PROMISE$V21)
defective <- replicate(nrow(PROMISE),0) # Defining defective vector
for (i in 1:nrow(PROMISE)) {
  if (PROMISE$V21[i] >= 1)
  {
    defective[i] = 1
  }
  else
  {
    defective[i] = 0
  }
}

PROMISE = PROMISE[c(-length(PROMISE))]
PROMISE = cbind(PROMISE,defective)
PROMISE$defective

########################################################
#Defining the predictors and outcome
predictors<-c("wmc","dit","noc","cbo","rfc","lcom","ca","ce",
              "npm","lcom3","loc","dam","moa","mfa","cam","ic",
              "cbm","amc","max_cc","avg_cc")
outcomeName<-'bug'
########################################################

########################################################
# Function Declarations Section
########################################################

# Declaring the training controls for multiple models
########################################################
# Adaptive Bootstrap Validation technique is used
########################################################
fitControl <- trainControl(method = "adaptive_boot", 
                           number = 50, 
                           savePredictions = 'final', 
                           classProbs = T)
########################################################

## Declaring the normalization function
########################################################
nor <-function(x) {(x -min(x))/(max(x)-min(x))}
########################################################

## Declaring the accuracy function
########################################################
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

########################################################
# Random Forest
########################################################
# Partition data training - 90%, test - 10%
set.seed(16)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainSetRF <- PROMISE[ind == 1,]
testSetRF <- PROMISE[ind == 2,]
########################################################
# Training the random forest model
model_rf<-train(trainSetRF[,c(1:20)], trainSetRF$defective, method='rf', trControl=fitControl, tuneLength=3)

# Predicting using random forest model
testSetRF$pred_rf <- predict(object = model_rf, testSetRF[,c(1:20)])
testSetRF$pred_rf <- ifelse(testSetRF$pred_rf > 0.001, "1", "0")

testSetRF$pred_rf <- as.factor(testSetRF$pred_rf)
testSetRF$defective <- as.factor(testSetRF$defective)

levels(testSet$defective)
levels(testSet$pred_rf)
# Checking the accuracy of the random forest model
confusionMatrix(testSetRF$defective,testSetRF$pred_rf)
########################################################


########################################################
# Logistic Regression Classifier -- Accuracy: 73.18653
########################################################
# Partition data training - 90%, test - 10%
set.seed(16)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainSetLR <- PROMISE[ind == 1,]
testSetLR <- PROMISE[ind == 2,]
########################################################
# Visualizations
x <- trainSetLR[,1:20]
y <- trainSetLR[,21]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#Training the Logistic regression model
model_lr <- glm(trainSetLR$defective ~ ., data = trainSetLR, family = binomial(link <- "logit"))
summary(model_lr)
#model_lr1<-train(trainSet[,c(1:20)], trainSet$defective, method='glm', trControl=fitControl, tuneLength=5)

#Predicting using knn model
pred_probslr <- predict(object = model_lr, newdata = testSetLR[,c(1:20)], type = "response")
pred_probslr
pred_lr <- ifelse(pred_probslr > 0.45, "1", "0")
cm_lr <- table(testSetLR$defective, pred_lr)
accuracy(cm_lr)
########################################################


########################################################
# k-NN ------- Accuracy: 73.51828 
########################################################
## Create a random number equal 90% of total number of rows
rand <- sample(1:nrow(PROMISE),0.9 * nrow(PROMISE))

## Defining the normalization function
knn_nor <- as.data.frame(lapply(PROMISE[,(1:20)], nor))
########################################################
## Training dataset extracted
trainSetKNN <- knn_nor[rand,]
## Test dataset extracted
testSetKNN <- knn_nor[-rand,]
## The 2nd column of training dataset because that is what we need to predict about testing dataset
## Also convert ordered factor to normal factor
knn_target <- as.factor(PROMISE[rand,21])

## The actual values of 2nd couln of testing dataset to compaire it with values that will be predicted
## Also convert ordered factor to normal factor
knn_test_target <- as.factor(PROMISE[-rand,21])
########################################################
#Training the knn model
#model_knn <- train(trainSet[,c(1:20)], trainSet$defective, method='knn', trControl=fitControl, tuneLength=3)
model_knn <- knn(train = trainSetKNN[,c(1:20)], test = testSetKNN[,c(1:20)], cl = knn_target, k = 20)

#Predicting using knn model
knn_confusionMatrix <- table(model_knn ,knn_test_target)

accuracy(knn_confusionMatrix) # 73.51828 
########################################################
# AUC
testSetKNN$pred_knn <- as.numeric(testSetKNN$pred_knn)
testSetKNN$defective <- as.numeric(testSetKNN$defective)

roc_knn <- roc(testSetKNN$defective, testSetKNN$pred_knn)
auc(roc_knn)

# Plotting the AUC
rectangle <- function(x, y, width, height, density=12, angle=-45, ...) 
  polygon(c(x,x,x+width,x+width), c(y,y+height,y+height,y), 
          density=density, angle=angle, ...)
roc_knn <- transform(roc_knn, dFPR = c(diff(FPR), 0),
                     dTPR = c(diff(TPR), 0))
plot(0:10/10, 0:10/10, type='n', xlab="FPR", ylab="TPR")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")
with(roc_knn, {
  mapply(rectangle, x=dFPR, y=0,   
         width=dFPR, height=dTPR, col="green", lwd=2)
  mapply(rectangle, x=dFPR, y=dTPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  
  lines(dFPR, dTPR, type='b', lwd=3, col="red")
})
########################################################


########################################################
# Support Vector Machines -- Accuracy: 71.50259
########################################################
# Partition data training - 90%, test - 10%
set.seed(16)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainSetSVM <- PROMISE[ind == 1,]
testSetSVM <- PROMISE[ind == 2,]

# Feature Scaling 
trainSetSVM[,c(1:20)] = scale(trainSetSVM[,c(1:20)]) 
testSetSVM[,c(1:20)] = scale(testSetSVM[,c(1:20)]) 

# Training SVM
model_svm = svm(formula = trainSetSVM$defective ~ ., 
                 data = trainSetSVM, 
                 type = 'C-classification', 
                 kernel = 'linear') 

# Predicting the Test set results 
pred_svm = predict(model_svm, newdata = testSetSVM[-21]) 

# Making the Confusion Matrix 
cm_svm = table(testSetSVM$defective, pred_svm)
accuracy(cm_svm) # 71.50259


# Need to modify the training and testing visualizations
# Plotting the training data set results 
set = trainSetSVM 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('Age', 'EstimatedSalary') 
y_grid = predict(classifier, newdata = grid_set) 

plot(set[, -3], 
     main = 'SVM (Training set)', 
     xlab = 'Age', ylab = 'Estimated Salary', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3')) 

set = test_set 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('Age', 'EstimatedSalary') 
y_grid = predict(classifier, newdata = grid_set) 

plot(set[, -3], main = 'SVM (Test set)', 
     xlab = 'Age', ylab = 'Estimated Salary', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3')) 
########################################################

########################################################
# Naive Bayes Classifier
########################################################
# Partition data training - 90%, test - 10%
set.seed(16)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainSet <- PROMISE[ind == 1,]
testSet <- PROMISE[ind == 2,]
########################################################

########################################################

########################################################
# Classification Tree (Decision Tree) Classifier  -- Accuracy: 72.40933
########################################################
# Partition data training - 90%, test - 10%
set.seed(16)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainSetCART <- PROMISE[ind == 1,]
testSetCART <- PROMISE[ind == 2,]
nrow(testSetCART)
nrow(trainSetCART)
########################################################
model_cart <- rpart(trainSetCART$defective ~ ., data = trainSetCART, method = "class")
par(xpd = NA)
plot(model_cart)
text(model_cart, digits = 3)
print(model_cart, digits = 3)

# Predictions -- Decision Tree
pred_cart <- predict(object = model_cart, testSetCART[,c(1:20)], type = "class")
cm_cart <- table(testSetCART$defective, pred_cart)
print(cm_cart)
accuracy(cm_cart)
########################################################

########################################################
# Decision Tree Pruning ---- not working
########################################################
# Fit the model on the training set 
set.seed(123)
model_cart1 <- train(trainSetCART$defective ~., data = trainSetCART, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 1)
# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model_cart1)
########################################################

########################################################
# Neural Network Model
########################################################
# Partition data training - 90%, test - 10%
set.seed(16)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainSet <- PROMISE[ind == 1,]
testSet <- PROMISE[ind == 2,]
########################################################


########################################################
# Weighing Methods -- Modify methods according to the developed model
########################################################
# Averaging
########################################################
#Predicting the probabilities
testSet$pred_rf_prob<-predict(object = model_rf,testSet[,predictors],type='prob')
testSet$pred_knn_prob<-predict(object = model_knn,testSet[,predictors],type='prob')
testSet$pred_lr_prob<-predict(object = model_lr,testSet[,predictors],type='prob')

#Taking average of predictions
testSet$pred_avg<-(testSet$pred_rf_prob$Y+testSet$pred_knn_prob$Y+testSet$pred_lr_prob$Y)/3

#Splitting into binary classes at 0.5
testSet$pred_avg<-as.factor(ifelse(testSet$pred_avg>0.5,'Y','N'))
########################################################

########################################################
# Majority Voting
########################################################
#Predicting the probabilities
testSet$pred_majority<-as.factor(ifelse(testSet$pred_rf=='Y' & testSet$pred_knn=='Y','Y',
                                        ifelse(testSet$pred_rf=='Y' & testSet$pred_lr=='Y','Y',
                                        ifelse(testSet$pred_knn=='Y' & testSet$pred_lr=='Y','Y','N'))))
########################################################

########################################################
# Weighted Average
########################################################
#Taking weighted average of predictions
testSet$pred_weighted_avg<-(testSet$pred_rf_prob$Y*0.25)+
                            (testSet$pred_knn_prob$Y*0.25)+
                            (testSet$pred_lr_prob$Y*0.5)

#Splitting into binary classes at 0.5
testSet$pred_weighted_avg<-as.factor(ifelse(testSet$pred_weighted_avg>0.5,'Y','N'))
########################################################

########################################################
# Performance Weighting
########################################################

########################################################

########################################################
# Bayesian Combination
########################################################

########################################################

########################################################
# Distribution Summation
########################################################

########################################################

########################################################
# Dempster–Shafer
########################################################

########################################################

########################################################
# Naïve Bayes
########################################################

########################################################

########################################################
# Entropy Weighting
########################################################

########################################################

########################################################
# Entropy Weighting
########################################################

########################################################

########################################################
# Logarithmic Opinion Pool
########################################################

########################################################