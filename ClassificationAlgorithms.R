# Classification Algorithms: 
# Categories: I) Statistical Classifiers
#            II) Nearest Neighbour Methods
#           III) Neural Networks
#            IV) SVM Based Classifiers
#             V) Decision Tree Approaches
#            VI) Ensemble Methods

#   I) Statistical Classifiers:
#------------------------------
#   I.1) LDA
#   I.2) QDA
#   I.3) Logistic Regression (LogReg)
#   I.4) Naive Bayes (NB)
#   I.5) Bayesian Networks (BayesNet)
#   I.6) Least Angle Regression (LARS)
#   I.7) Relevance Vector Machine (RVM)

#  II) Nearest Neighbour Methods:
#--------------------------------
#  II.1) K-Nearest Neighbour (K-NN)
#  II.2) K-Star (K*)

# III) Neural Networks:
#----------------------
# III.1) MLP (Try Deep Neural Networks)
# III.2) RBF Netwroks (RBFNet)

#  IV) SVM Based Classifiers:
#----------------------------
#  IV.1) SVM
#  IV.2) Lagrangian SVM (L-SVM)
#  IV.3) Least Squares SVM (LS-SVM)
#  IV.4) Linear Programming (LP)
#  IV.5) Voted Perceptron (VP)

#   V) Decision Tree Approaches:
#-------------------------------
#   V.1) C 4.5 Decision Tree (C 4.5)
#   V.2) Classification and Regression Tree (CART)
#   V.3) Alternating Decision Tree (ADT)

#  VI) Ensemble Methods:
#-----------------------
#  VI.1) Random Forest (RandFor)
#  VI.2) Logistic Model Tree (LMT)
------------------------------------------------
------------------------------------------------
  
##   I) Statistical Classifiers:
#------------------------------
#   I.1) LDA (Scolding that, cannot allocate vector of size 967.6 Mb)
linearDA <- lda(trainLarge$defective~.,trainLarge)
attributes(linearDA)

#  II) Nearest Neighbour Methods:
#--------------------------------
#  II.1) K-Nearest Neighbour (K-NN)
str(trainLarge)
table(trainLarge$defective)

# Normalizing the data (Everything comes in range [0,1])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
NormTrainLarge <- as.data.frame(lapply(PROMISE[,c(2:21)], normalize))

# Preparing Training data and testing data.
SampleTrainData <- NormTrainLarge[ind == 1,]
SampleTestData <- NormTrainLarge[ind == 2,]

# Preparing Training Lables and testing Lables.
TrainLables <- PROMISE[ind == 1,22]
TestLables <- PROMISE[ind == 2,22]
str(TrainLables)

# Training with K-NN
knn_Model <- knn(train = SampleTrainData, test = SampleTestData, cl = TrainLables , k = 10)
knn_Model
summary(knn_Model)
length(TestLables)

confusionMatrix(knn_Model, TestLables)
CrossTable(x = TestLables, y = knn_Model, digits = 3, max.width = 5, expected = TRUE, 
           prop.r = TRUE, prop.c = TRUE, prop.t = TRUE, prop.chisq = TRUE, chisq = FALSE, 
           fisher = TRUE, missing.include = TRUE, resid = TRUE)

# III) Neural Netwrk methods
#---------------------------
# III.1) MLP
mlpData <- PROMISE[,c(2:21)]
mlpTargets <- PROMISE$defective
mlpTraining <- splitForTrainingAndTest(mlpData, mlpTargets,ratio = 0.2)
mlpTraining
mlpTraining <- normTrainingAndTestSet(mlpTraining)
nnModel <- mlp(mlpTraining$inputsTrain, mlpTraining$targetsTrain, size = 5, learnFuncParams = c(0,1),
               maxit = 80, inputsTest = mlpTraining$inputsTest, targetsTest = mlpTraining$targetsTest)

summary(nnModel)

weightMatrix(nnModel)
extractNetInfo(nnModel)

par(mfrow=c(2,2))
plotIterativeError(nnModel)

#  Prediction using MLP
mlpPrediction <- predict(nnModel, mlpTraining$inputsTest)
plotRegressionError(mlpPrediction,mlpTraining$targetsTest)
as.factor(fitted.values(nnModel))
as.factor(mlpTraining$targetsTrain)
confusionMatrix(as.factor(mlpTraining$targetsTrain),as.factor(fitted.values(nnModel)))
confusionMatrix(mlpTraining$targetsTest,mlpPrediction)

plotROC(fitted.values(nnModel), mlpTraining$targetsTrain)
plotROC(mlpPrediction, mlpTraining$targetsTest)

#confusion matrix with 402040-method
confusionMatrix(mlpTraining$targetsTrain, encodeClassLabels(fitted.values(nnModel),method="402040", l=0.4, h=0.6))

# III.2) Neural Networks

## Scale data for neural network

max = apply(PROMISE[,c(2:22)] , 2 , max)
min = apply(PROMISE[,c(2:22)], 2 , min)

scaled = as.data.frame(scale(PROMISE[,c(2:22)], center = min, scale = max - min))

# creating training and test set
trainNN = scaled[ind , ]
testNN = scaled[-ind , ]

# fit neural network
set.seed(16)
NN = neuralnet(trainNN$defective~., trainNN, hidden = 3 , linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network
predict_testNN = compute(NN, testNN[,c(1:20)])
predict_testNN$net.result

# Converting probabilities into binary classes setting threshold level 0.5
probs <- predict_testNN$net.result
predNN <- ifelse(probs>0.5,0,1)
predNN

plot(testNN$defective, predNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((PROMISE$defective - predict_testNN)^2) / nrow(PROMISE)) ^ 0.5

# III.3) Radial Basis Function
rbfData <- PROMISE[,c(2:21)]
rbfOutput <- rbf()

#---------------------------------
# V) Decision Tree based methods
#---------------------------------

# 1. C 4.5
#---------------------------------
# fit model
str(treeTrain)
nrow(trainLarge)
nrow(SampleTrainData)
treeTrain <- trainLarge[,c(2:21)]
c45fit <- J48(treeTrain$defective~., data=treeTrain)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)

# 2. CART
#---------------------------------
# fit model
fit <- rpart(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)

# 3. PART
#---------------------------------
# fit model
fit <- PART(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)

# 4. Bagging CART
#---------------------------------
# fit model
fit <- bagging(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)

# 5. Random Forest
#---------------------------------
# fit model
fit <- randomForest(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)

# 6. Gradient Boosted Machine
#---------------------------------
# fit model
fit <- gbm(Species~., data=iris, distribution="multinomial")
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, iris)
# summarize accuracy
table(predictions, iris$Species)

# 7. Training the Decision Tree classifier with criterion as information gain
set.seed(24)
dtreeFit <- train(trainLarge$defective~., data = trainLarge[,c(2:21)], 
                  method = "rpart", parms = list(split = "information"),
                  trctrl = valiBS632, tuneLength = 10)
