#----------------Om NamahShivaaya-----------------#
#----------------ShivaayaNamah Om-----------------#

# Setting the path to add libraries 
setwd('/media/pramod/Education/Research/May2-2019/Sharma/Work/Programs/R/')
source("Libraries.R")

# Data collection: Merging all individual files (Which contain class level metrics), totalling to 34.
setwd('/media/pramod/Education/Research/May2-2019/Sharma/Work/DataSets/PROMISE/')

# Reading all file names from the current directory
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){read.csv(i,header=FALSE, skip=1)})
PROMISE <- do.call(rbind.data.frame, All)

# Here in this space, I've to write a code to read original names of the attributes and assign it to PROMISE dataset.

# Getting to know whether the module is defective or not
PROMISE$V22 <- as.numeric(PROMISE$V22)
PROMISE$V22 <- PROMISE$V22-1
defective <- replicate(nrow(PROMISE),0) # Defining defective vector
for (i in 1:nrow(PROMISE)) {
  if (PROMISE$V22[i] >= 1)
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

# Arrangement for plotting
# For each level (#bugs), assign one color to each category. 
colors = rainbow(length(unique(PROMISE$defective)))
names(colors) = unique(PROMISE$defective)
names(colors)

# Partition data training - 90%, test - 10%
set.seed(16)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainLarge <- PROMISE[ind == 1,]
testLarge <- PROMISE[ind == 2,]
nrow(trainLarge)
str(trainLarge)

# Visualizing the input data
# 1. Histogram plots
par(mfrow=c(2,21))
for(i in 2:21) {
  hist(trainLarge[,i], main=names(trainLarge)[i])
}
train()
# 2. Boxplots
par(mfrow=c(2,21))
for(i in 2:21) {
  boxplot(trainLarge[,i], main=names(trainLarge)[i])
}

# 3. Missingness Map
missmap(trainLarge[,c(2:21)], col=c("blue", "red"), legend=FALSE)

# 4. correlation Matrix
correlations <- cor(trainLarge[,2:21])
corrplot(correlations, method="circle")

# 5. Scatterplot Matrix
pairs(trainLarge, col=trainLarge$defective)

# 6. Density Plots
x<- trainLarge[,c(2:21)]
y<- DepVar
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Dimensionality Reduction for training
# 1. PCA
PCAdata <- trainLarge
PCAonLargeData <- princomp(PCAdata[,c(2:21)], scores = TRUE)
summary(PCAonLargeData)

# Plot the most variance principal components
jpeg("/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE-stuff/PCA/pcadata.jpg")
screeplot(PCAonLargeData)
dev.off()

DepVar = trainLarge$defective
DepVar<-as.factor(DepVar)

# Selecting top ten attributes which produce high variance of the original data
PCscores <- PCAonLargeData$scores[,c(1:10)]
summary(PCscores)
ScoresData <- data.frame(PCscores, DepVar)
summary(ScoresData)

# 2. t-SNE
## Executing the t-SNE algorithm on curated data
tsne <- Rtsne(trainLarge[,c(2:21)], dims = 3, 
              perplexity=50, max_iter = 1000, 
              check_duplicates = FALSE)
summary(tsne$Y)

# To know system execution time to perfome t-SNE
exeTimeTsne <- system.time(Rtsne(trainLarge[,c(-1,-22)], dims = 3,
                                 perplexity=100, max_iter = 3000,
                                 check_duplicates = FALSE))
exeTimeTsne

## Plotting
jpeg("/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE-stuff/t-SNE/t-SNE_p50_it1000_D1.jpg")
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=DepVar, col=colors[DepVar])
dev.off()

# Feature Selection before training
# 1. Boruta
#trainData <- read.csv(file = "2.csv", header = TRUE, sep = ",")
#trainData <- trainData[-1]
#names(trainData)
set.seed(16)
BorutaFS <- Boruta(trainLarge$defective~., data = trainLarge, doTrace = 2)
print(BorutaFS)

# Ploting
plot(BorutaFS, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(BorutaFS$ImpHistory),function(i)
  BorutaFS$ImpHistory[is.finite(BorutaFS$ImpHistory[,i]),i])
names(lz) <- colnames(BorutaFS$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las = 2,labels = names(Labels), at = 1:ncol(BorutaFS$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(BorutaFS)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

# 2. Relief (System getting struck while runnig this function)
ReliefWeights <- relief(trainLarge$defective ~., data = trainLarge[,c(2:21)], neighbours.count = 5, sample.size = 20)
summary(ReliefWeights)
ReliefWeights
# 3 Variable Importance
# 3.1 Using Logistic Regression
glmFit <- glm(trainLarge$defective~., trainLarge[,c(2:21)], family = "binomial")

# Create an importance based on mean decreasing gini
varImp(glmFit)

# 3.2 Using Random Forest
rfFit <- randomForest(trainLarge$defective~., data = trainLarge[,c(2:21)], importance = TRUE)
rfpred <- predict(rfFit, testLarge[,c(2:21)])
table(rfpred)

# Checking the accuracy
auc(rfpred, testLarge$defective)

# Create an importance based on mean decreasing gini
importance(rfFit)
varImp(rfFit)

# Create a plot of importance scores by random forest
varImpPlot(rfFit)


#------------------------------------------------------------------#
#--------------------Training the Classifiers----------------------#
#------------------------------------------------------------------#

# 1. Support Vector Machines

# 1.1 SVM Training without using dimensionality reduction
SVMLarge1 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "linear", 
                     cost = 10, scale = TRUE, probabilities = TRUE)
SVMLarge0 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "linear", 
                     cost = 10, scale = FALSE, probabilities = TRUE)
SVMradial1 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "radial", 
                      cost = 10, scale = TRUE, probabilities = TRUE)
SVMradial0 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "radial", 
                      cost = 10, scale = FALSE, probabilities = TRUE)
SVMsigmoid1 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "sigmoid", 
                       cost = 10, scale = TRUE, probabilities = TRUE)
SVMsigmoid0 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "sigmoid",
                       cost = 10, scale = FALSE, probabilities = TRUE)
SVMpolynomial1 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "polynomial", 
                          cost = 10, scale = TRUE, probabilities = TRUE)
SVMpolynomial0 <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "polynomial", 
                          cost = 10, scale = FALSE, probabilities = TRUE)

# 1.2 SVM Training using t-SNE on Large Data.
tSNEScores <- tsne$Y
summary(tSNEScores)
tSNEData <- data.frame(tSNEScores, DepVar)
SVMtSNELarge1 <- svm(DepVar ~ ., data = tSNEData, kernel = "linear", 
                     cost = 10, scale = TRUE, probabilities = TRUE)
SVMtSNELarge0 <- svm(DepVar ~ ., data = tSNEData, kernel = "linear", 
                     cost = 10, scale = FALSE, probabilities = TRUE)
SVMtSNEradial1 <- svm(DepVar ~ ., data = tSNEData, kernel = "radial", 
                      cost = 10, scale = TRUE, probabilities = TRUE)
SVMtSNEradial0 <- svm(DepVar ~ ., data = tSNEData, kernel = "radial", 
                      cost = 10, scale = FALSE, probabilities = TRUE)
SVMtSNEsigmoid1 <- svm(DepVar ~ ., data = tSNEData, kernel = "sigmoid", 
                       cost = 10, scale = TRUE, probabilities = TRUE)
SVMtSNEsigmoid0 <- svm(DepVar ~ ., data = tSNEData, kernel = "sigmoid",
                       cost = 10, scale = FALSE, probabilities = TRUE)
SVMtSNEpolynomial1 <- svm(DepVar ~ ., data = tSNEData, kernel = "polynomial", 
                          cost = 10, scale = TRUE, probabilities = TRUE)
SVMtSNEpolynomial0 <- svm(DepVar ~ ., data = tSNEData, kernel = "polynomial", 
                          cost = 10, scale = FALSE, probabilities = TRUE)

# 1.3 SVM Training using PCA on Large Data.
SVMPCA1 <- svm(DepVar ~ ., data = ScoresData, kernel = "linear", 
               cost = 10, scale = TRUE, probabilities = TRUE, cross = 10)
SVMPCARadial1 <- svm(DepVar ~ ., data = ScoresData, kernel = "radial",
                     cost = 10, scale = TRUE, probabilities = TRUE)
SVMPCASigmoid1<- svm(DepVar ~ ., data = ScoresData, kernel = "sigmoid", 
                     cost = 10, scale = TRUE, probabilities = TRUE)
SVMPCAPolynomial1 <- svm(DepVar ~ ., data = ScoresData, kernel = "polynomial",
                         cost = 10, scale = TRUE, probabilities = TRUE)
SVMPCA0 <- svm(DepVar ~ ., data = ScoresData, kernel = "linear",
               cost = 10, scale = FALSE, probabilities = TRUE)
SVMPCARadial0 <- svm(DepVar ~ ., data = ScoresData, kernel = "radial",
                     cost = 10, scale = FALSE, probabilities = TRUE)
SVMPCASigmoid0 <- svm(DepVar ~ ., data = ScoresData, kernel = "sigmoid",
                      cost = 10, scale = FALSE, probabilities = TRUE)
SVMPCAPolynomial0 <- svm(DepVar ~ ., data = ScoresData, kernel = "polynomial", 
                         cost = 10, scale = FALSE, probabilities = TRUE)

# 2. Naive Bayes Classifier
# 2.1 Naive  Bayes without dimensionality reduction
nb <- naiveBayes(DepVar~., data = trainLarge[,c(2:21)])

# 2.2 Naive  Bayes using t-SNE
nbtSNE <- naiveBayes(DepVar~., data =tSNEData)

# 2.3 Naive  Bayes using PCA
nbPCA <- naiveBayes(DepVar~., data =ScoresData)

# 3. Logistic Regression Classifier
# 3.1 Logistic Regression without Dimensionality Reduction
LRLarge <- glm(DepVar ~ ., data <- trainLarge[,c(2:21)], family = binomial(link <- "logit"))
summary(LRLarge)

# 3.2 Logistic Regression implemented on t-SNE'ed data
LRtSNELarge <- glm(DepVar ~ ., data <- tSNEData, family = binomial(link <- "logit"))
summary(LRtSNELarge)

# 3.3 Logistic Regression implemented on PCA'ed data
LRPCALarge <- glm(DepVar ~ ., data <- ScoresData, family = binomial(link <- "logit"))
summary(LRPCALarge)

# 4. k-NN

# 5. Neural Network Model

#------------------------------------------------------------------#
#----------------------Testing Procedure---------------------------#
#------------------------------------------------------------------#

#   I) Apply Dimensionality Reduction Algorithms on seperated test dataset
#  II) Use trained models to predict the test data lables
# III) Calculate and analyze the accuracies

# Dimensionality Reduction for testing data
# 1. PCA
PCAonLargeDataTest <- princomp(testLarge[,c(2:21)], scores = TRUE)
summary(PCAonLargeDataTest)
PCscoresTest <- PCAonLargeDataTest$scores[,c(1:10)]
summary(PCscoresTest)

# 2. t-SNE
## Executing the t-SNE algorithm on curated data
tsneTest <- Rtsne(testLarge[-1], dims = 2, perplexity=50, max_iter = 1000, check_duplicates = FALSE)
summary(tsneTest$Y)

# Prediction Tasks

# 1.1 SVM -- PCA
# Prediction using SVM with PCA (Getting worst performance on PROMISE. Need to revisit the factors affecting the performance.)
SVMPPCAlinear1 <- predict(SVMPCA1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
PSVMPCAradial1 <- predict(SVMPCARadial1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
PSVMPCAsigmoid1 <- predict(SVMPCASigmoid1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
PSVMPCAploy1 <- predict(SVMPCAPolynomial1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
SVMPPCAlinear0 <- predict(SVMPCA0,PCscoresTest, decision.values = TRUE, probabilities = FALSE)
PSVMPCAradial0 <- predict(SVMPCARadial0,PCscoresTest, decision.values = TRUE, probabilities = FALSE)
PSVMPCAsigmoid0 <- predict(SVMPCASigmoid0,PCscoresTest, decision.values = TRUE, probabilities = FALSE)
PSVMPCAploy0 <- predict(SVMPCAPolynomial0,PCscoresTest, decision.values = TRUE, probabilities = FALSE)
summary(SVMPPCAlinear1)
summary(PSVMtPCAradial1)
summary(PSVMPCAsigmoid1)
summary(PSVMPCAploy1)

# Calculating the accuracies
confusionMatrix(as.factor(SVMPPCAlinear0),as.factor(testLarge$defective)) # Accuracy: 85.27
confusionMatrix(as.factor(PSVMPCAradial0),as.factor(testLarge$defective)) # Accuracy: 68.53
confusionMatrix(as.factor(PSVMPCAsigmoid0),as.factor(testLarge$defective)) # Accuracy: 58.52
confusionMatrix(as.factor(PSVMPCAploy0),as.factor(testLarge$defective)) # Accuracy: 66.69
confusionMatrix(as.factor(SVMPPCAlinear1),as.factor(testLarge$defective)) # Accuracy: 85.27
confusionMatrix(as.factor(PSVMPCAradial1),as.factor(testLarge$defective)) # Accuracy: 68.53
confusionMatrix(as.factor(PSVMPCAsigmoid1),as.factor(testLarge$defective)) # Accuracy: 58.52
confusionMatrix(as.factor(PSVMPCAploy1),as.factor(testLarge$defective)) # Accuracy: 66.69

# 1.2 SVM -- t-SNE
PSVMtSNElinear1 <- predict(SVMtSNELarge1,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEradial1 <- predict(SVMtSNEradial1,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEsigmoid1<- predict(SVMtSNEsigmoid1,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEploy1 <- predict(SVMtSNEpolynomial1,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNElinear0 <- predict(SVMtSNELarge0,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEradial0 <- predict(SVMtSNEradial0,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEsigmoid0 <- predict(SVMtSNEsigmoid0,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEploy0 <- predict(SVMtSNEpolynomial0,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
summary(PSVMtSNElinear)
summary(PSVMtSNEradial)
summary(PSVMtSNEsigmoid)
summary(PSVMtSNEploy)

# Calculating the accuracies
confusionMatrix(as.factor(PSVMtSNElinear1),as.factor(testLarge$defective)) # Accuracy: 85.27
confusionMatrix(as.factor(PSVMtSNEradial1),as.factor(testLarge$defective)) # Accuracy: 65.09
confusionMatrix(as.factor(PSVMtSNEsigmoid1),as.factor(testLarge$defective)) # Accuracy: 61.19
confusionMatrix(as.factor(PSVMtSNEploy1),as.factor(testLarge$defective)) # Accuracy: 39.19
confusionMatrix(as.factor(PSVMtSNElinear0),as.factor(testLarge$defective)) # Accuracy: 85.27
confusionMatrix(as.factor(PSVMtSNEradial0),as.factor(testLarge$defective)) # Accuracy: 65.09
confusionMatrix(as.factor(PSVMtSNEsigmoid0),as.factor(testLarge$defective)) # Accuracy: 61.19
confusionMatrix(as.factor(PSVMtSNEploy0),as.factor(testLarge$defective)) # Accuracy: 39.19

# 2. Naive  Bayes

# 3. Logistic Regression
# 3.1 Prediction for testdata without using dimensionality reduction
nrow(testLarge)
LRProbs <- predict(LRLarge, newdata = testLarge[,c(2:21)], type = "response")
LRPred <- ifelse(lrProbs > 0.5, "1", "0")
mean(LRPred == testLarge[,c(2:21)])

# Confusion matrix
table(LRPred,testLarge$defective)

# Solving Overfitting (Not getting better performance with CK metrics as features)
# Fittig a smaller model using CK Metrics (First trial)
# Try using Feature selection approaches

LRSmall= glm(trainLarge$defective~., data = trainLarge[,c(2:7)], family = binomial)
LRSmallProb = predict(LRSmall, newdata = testLarge[,c(2:7)], type = "response")
LRSmallProb[1:10]
LRSmallPred = ifelse(LRSmallProb > 0.9, "0", "1") # Tried with different threshold values.
# Need to estimate precision
table(LRSmallPred,testLarge$defective)
confusionMatrix(as.factor(LRSmallPred), as.factor(testLarge$defective))

# 3.2 Prediction for testdata using t-SNE
LRtSNEProb <- predict(LRtSNELarge, newdata = testLarge[,c(2:21)], type = "response")
LRtSNEPred <- ifelse(lrtSNELinearProb > 0.09, "1", "0")

# 3.3 Prediction for testdata using PCA
LRPCAprob <- predict(LRtSNELarge, newdata = testLarge[,c(2:21)], type = "response")
LRPCApred <- ifelse(LRPCAprob > 0.01, "1", "0")

# Validation Techniques
# 1. Cross Validation Methods
# 1.1 Regular Cross Validation:
valiRCV <- trainControl(method = "cv", number = 100)
# 1.2 Repeated Cross Validation:
valiRCV <- trainControl(method = "repeatedcv", number = 100, repeats = 10)
# 1.3 Adaptive CV:
valiACV <- trainControl(method = "adaptive_cv", number = 100, repeats = 10)
# 1.4 Leave OneOut CV:
valiLOOCV <- trainControl(method = "LOOCV", number = 100)
# 1.5 Leave Group Out CV:
valiLGOCV <- trainControl(method = "LGOCV", number = 100)
# 1.6 Adaptive Leave Group Out CV:
valiALGOCV <- trainControl(method = "adaptive_LGOCV", number = 100, p = 0.8)

# 2. Bootstrap Methods
# 2.1 Regular Bootstrap:
valiBS <- trainControl(method = "boot", number = 100)
# 2.2 Bootstrap 0.632:
valiBS632 <- trainControl(method = "boot632", number = 100)
# 2.3 Optimism Bootstrap:
valiOBS <- trainControl(method = "optimism_boot", number = 100)
# 2.4 Bootstrap All:
valiBSAll <- trainControl(method = "boot_all", number = 100)
# 2.5 Adaptive Bootstrap:
valiABS <- trainControl(method = "adaptive_boot", number = 100)