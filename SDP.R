# Setting the path
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/Programs/R/')
source("Libraries.R")
# Data collection: Merging all individual files (Which contain class level metrics), totalling to 34.
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE/')
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){read.csv(i,header=FALSE, skip=1)})
PROMISE <- do.call(rbind.data.frame, All)
#write.csv(PROMISE,"PROMISE.csv", row.names=FALSE)
names(PROMISE) # Total 22 variables are present in the dataset with the name Vi(i:1->22)

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
PROMISE$defective

# Run this for 1000 times and record accuracies
# Partition data training - 90%, test - 10%
set.seed(555)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainLarge <- PROMISE[ind == 1,]
testLarge <- PROMISE[ind == 2,]
nrow(trainLarge)
str(trainLarge)
#pairs.panels(trainLarge[2:21], gap = 0, bg = c("red","green")[trainLarge$defective], pch = 21)

# Arrangement for plotting
# For each level (#bugs), assign one color to each category. 
colors = rainbow(length(unique(trainLarge$defective)))
names(colors) = unique(trainLarge$defective)
names(colors)

# Dimensionality Reduction Algorithms: Linear Dimensionality Reductions and Non-Linear Reductions
# 1. PCA (Linear)
# 2. Kernel PCA
# 3. Nonlinear PCA
# 4. LLE (Locally Linear Embedding)
# 5. t-SNE
# 6. Isomap
# 7. Sammon Mapping
# 8. Local Tangent Space Analysis
# 9. Modified LLE
# 10 Hessian LLE
# 11 Laplasian Eigenmaps
# 12 CCA (Curvilinear Component Analysis)
# 13 CDA (Curvilinear Distance Analysis)
# 14 MVU (Maximum Variance Unfolding)
# 15 Classical MDS (Linear)
# 16 Multilayer Autoencoders
# 17 Diffusion Maps
# 18 Locally Linear Coordination
# 19 Manifold Charting
# 20 Factor Analysis (Linear)

# Dimensionality Reduction for training
# 1. PCA
PCAdata <- trainLarge
PCAonLargeData <- princomp(PCAdata[,c(2:21)], scores = TRUE)

# Plot the most variance principal components
jpeg("/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE-stuff/pcadata.jpg")
screeplot(PCAonLargeData)
dev.off()

summary(PCAonLargeData)
DepVar = trainLarge$defective
class(DepVar)
DepVar<-as.factor(DepVar)
PCscores <- PCAonLargeData$scores[,c(1:10)]
summary(PCscores)
ScoresData <- data.frame(PCscores, DepVar)
summary(ScoresData)

# 5. t-SNE
## Executing the t-SNE algorithm on curated data
tsne <- Rtsne(trainLarge[-1], dims = 3, perplexity=50, max_iter = 1000, check_duplicates = FALSE)
summary(tsne$Y)

# To know system execution time to perfome t-SNE
exeTimeTsne <- system.time(Rtsne(trainLarge[-1], dims = 3, perplexity=100, max_iter = 3000, check_duplicates = FALSE))
exeTimeTsne

## Plotting
jpeg("t-SNE_p50_it1000_D1.jpg")
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=trainLarge$defective, col=colors[trainLarge$defective])
dev.off()

# Training the Classifiers
# 1.1 SVM Training using t-SNE on Large Data with 3 dimensions.
tSNEScores <- tsne$Y
summary(tSNEScores)
tSNEData <- data.frame(tSNEScores, DepVar)
SVMtSNELarge1 <- svm(DepVar ~ ., data = tSNEData, kernel = "linear", cost = 10, scale = TRUE)
SVMtSNELarge0 <- svm(DepVar ~ ., data = tSNEData, kernel = "linear", cost = 10, scale = FALSE)
SVMtSNEradial1 <- svm(DepVar ~ ., data = tSNEData, kernel = "radial", cost = 10, scale = TRUE)
SVMtSNEradial0 <- svm(DepVar ~ ., data = tSNEData, kernel = "radial", cost = 10, scale = FALSE)
SVMtSNEsigmoid1 <- svm(DepVar ~ ., data = tSNEData, kernel = "sigmoid", cost = 10, scale = TRUE)
SVMtSNEsigmoid0 <- svm(DepVar ~ ., data = tSNEData, kernel = "sigmoid", cost = 10, scale = FALSE)
SVMtSNEpolynomial1 <- svm(DepVar ~ ., data = tSNEData, kernel = "polynomial", cost = 10, scale = TRUE)
SVMtSNEpolynomial0 <- svm(DepVar ~ ., data = tSNEData, kernel = "polynomial", cost = 10, scale = FALSE)

print(SVMtSNELarge0)
print(SVMtSNELarge1)
print(SVMtSNEradial0)
print(SVMtSNEradial1)
print(SVMtSNEWithPCAradial1)
print(SVMtSNEsigmoid0)
print(SVMtSNEsigmoid1)
print(SVMtSNEpolynomial0)
print(SVMtSNEpolynomial1)

# 1.2 SVM Training using PCA on Large Data with 3 dimensions.
SVMPCA1 <- svm(DepVar ~ ., data = ScoresData, kernel = "linear", cost = 10, scale = TRUE, probabilities = TRUE)
SVMPCARadial1 <- svm(DepVar ~ ., data = ScoresData, kernel = "radial", cost = 10, scale = TRUE, probabilities = TRUE)
SVMPCASigmoid1<- svm(DepVar ~ ., data = ScoresData, kernel = "sigmoid", cost = 10, scale = TRUE, probabilities = TRUE)
SVMPCAPolynomial1 <- svm(DepVar ~ ., data = ScoresData, kernel = "polynomial", cost = 10, scale = TRUE, probabilities = TRUE)
SVMPCA0 <- svm(DepVar ~ ., data = ScoresData, kernel = "linear", cost = 10, scale = FALSE)
SVMPCARadial0 <- svm(DepVar ~ ., data = ScoresData, kernel = "radial", cost = 10, scale = FALSE)
SVMPCASigmoid0 <- svm(DepVar ~ ., data = ScoresData, kernel = "sigmoid", cost = 10, scale = FALSE)
SVMPCAPolynomial0 <- svm(DepVar ~ ., data = ScoresData, kernel = "polynomial", cost = 10, scale = FALSE)

print(SVMPCALarge1)
print(SVMPCALargeRadial1)
print(SVMPCALargeSigmoid1)
print(SVMPCALargePolynomial1)
print(SVMPCALarge0)
print(SVMPCALargeRadial0)
print(SVMPCALargeSigmoid0)
print(SVMPCALargePolynomial0)

# 1.3 SVM Training without Dimensionality reduction
SVMLarge <- svm(DepVar ~ ., data = trainLarge[,c(2:21)], kernel = "linear", cost = 10, scale = FALSE)
summary(SVMLarge)
print(SVMLarge)
plot(SVMtSNELarge1, tSNEData, DepVar~.)

# 2. Logistic Regression
# 2.1 Logistic Regression using t-SNE
LRtSNELarge <- glm(DepVar ~ ., data <- tSNEData, family = binomial(link <- "logit"))
LRtSNELarge
summary(LRtSNELarge)

# 2.2 Logistic Regression using PCA
LRPCALarge <- glm(DepVar ~ ., data <- ScoresData, family = binomial(link <- "logit"))
LRPCALarge

# 2.3 Logistic Regression without Dimensionality Reduction
LRLarge <- glm(DepVar ~ ., data <- trainLarge[,c(2:21)], family = binomial(link <- "logit"))
LRLarge

# Testing Procedure
#   I) Apply Dimensionality Reduction Algorithms on seperated test set
#  II) Use trained models to predict the test data lables
# III) Calculate and analyze the accuracies

# Dimensionality Reduction for testing data
# 1. PCA
PCAonLargeDataTest <- princomp(testLarge[,c(2:21)], scores = TRUE)
summary(PCAonLargeDataTest)
PCscoresTest <- PCAonLargeDataTest$scores[,c(1:10)]
summary(PCscoresTest)

# 5. t-SNE
## Executing the t-SNE algorithm on curated data
tsneTest <- Rtsne(testLarge[-1], dims = 2, perplexity=50, max_iter = 1000, check_duplicates = FALSE)
summary(tsneTest$Y)

# Prediction Tasks

# 1. SVM -- PCA
# Prediction using SVM with PCA (Getting worst performance on PROMISE. Need to revisit the factors affecting the performance.)
summary(as.factor(testLarge$defective))
SVMPPCAlinear <- predict(SVMPCA1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
PSVMPCAradial <- predict(SVMPCARadial1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
PSVMPCAsigmoid <- predict(SVMPCASigmoid1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
PSVMPCAploy <- predict(SVMPCAPolynomial1,PCscoresTest, decision.values = TRUE, probabilities = TRUE)
summary(SVMPPCAlinear)
summary(PSVMtPCAradial)
summary(PSVMPCAsigmoid)
summary(PSVMPCAploy)

# Calculating the accuracies
summary(as.factor(testLarge$defective))
confusionMatrix(as.factor(SVMPPCAlinear),as.factor(testLarge$defective)) # Accuracy: 85.27
confusionMatrix(as.factor(PSVMPCAradial),as.factor(testLarge$defective)) # Accuracy: 68.53
confusionMatrix(as.factor(PSVMPCAsigmoid),as.factor(testLarge$defective)) # Accuracy: 58.52
confusionMatrix(as.factor(PSVMPCAploy),as.factor(testLarge$defective)) # Accuracy: 66.69

# 2. SVM -- t-SNE
PSVMtSNElinear <- predict(SVMtSNELarge1,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEradial <- predict(SVMtSNEradial1,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEsigmoid <- predict(SVMtSNEsigmoid1,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
PSVMtSNEploy <- predict(SVMtSNEpolynomial0,tsneTest$Y, decision.values = TRUE)#, probability = TRUE)
summary(PSVMtSNElinear)
summary(PSVMtSNEradial)
summary(PSVMtSNEsigmoid)
summary(PSVMtSNEploy)

# Calculating the accuracies
summary(as.factor(testLarge$defective))
confusionMatrix(as.factor(PSVMtSNElinear),as.factor(testLarge$defective)) # Accuracy: 85.27
confusionMatrix(as.factor(PSVMtSNEradial),as.factor(testLarge$defective)) # Accuracy: 65.09
confusionMatrix(as.factor(PSVMtSNEsigmoid),as.factor(testLarge$defective)) # Accuracy: 61.19
confusionMatrix(as.factor(PSVMtSNEploy),as.factor(testLarge$defective)) # Accuracy: 39.19

# Validation Techniques
# 1. Cross Validation Methods
# 1.1 Regular Cross Validation:
valiRCV <- trainControl(method = "cv", number = 100)
# 1.2 Repeated Cross Validation:
valiRCV <- trainControl(method = "repeatedcv", number = 100, repeats = 3)
# 1.3 Adaptive CV:
valiACV <- trainControl(method = "adaptive_cv", number = 100, repeats = 5)
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