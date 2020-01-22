# Setting the path
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/Programs/R/')
source("Libraries.R")
# Data collection: Merging all individual files (Which contain class level metrics), totalling to 34.
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE/')
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){read.csv(i, header=FALSE, skip=1, fileEncoding = "latin1", stringsAsFactors = FALSE)})
#All
PROMISE <- do.call(rbind.data.frame, All)
#write.csv(PROMISE,"PROMISE.csv", row.names=FALSE)
names(PROMISE) # Total 22 variables are present in the dataset with the name Vi(i:1->22)

## Curating the database for analysis with both t-SNE and PCA
Labels<-PROMISE$V22
PROMISE$V22<-as.factor(PROMISE$V22)
PROMISE$V22

# Getting to know whether the module is defective or not
PROMISE$V22 <- as.numeric(PROMISE$V22)
PROMISE$V22 <- PROMISE$V22-1
defective <- replicate(nrow(PROMISE),0)
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

# Partition data training - 90%, test - 10%
set.seed(555)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.9,0.1))
trainLarge <- PROMISE[ind == 1,]
testLarge <- PROMISE[ind == 2,]
nrow(trainLarge)
str(trainLarge)

# Arrangement for plotting
# For each level (#bugs), assign one color to each category. 
colors = rainbow(length(unique(PROMISE$defective)))
names(colors) = unique(PROMISE$defective)
names(colors)
DepVar <- PROMISE$defective
#PROMISE <- PROMISE[,c(2:21)]

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

# 1. PCA
PCAdata <- PROMISE[,c(2:21)]
summary(PCAdata)
PCAonLargeData <- princomp(PCAdata,scores = TRUE)
summary(PCAonLargeData)
PCscores <- PCAonLargeData$scores[,c(1:10)]
summary(PCscores)
ScoresData <- data.frame(PCscores, DepVar <- as.factor(DepVar))

# 2. Kernel PCA (Taking huge amount of time) (Asking memory for a vector of size 1.3Gb)
kPCAdata <- PROMISE[,c(2:21)]
summary(kPCAdata)
kPCAonLargeData <- kpca(~.,data = kPCAdata, kernel = "rbfdot", kpar = list(sigma = 0.2), features = 2)

# 3. Non-Linear PCA (Taking huge amount of time) (PCA ⊆ NLPCA)
nlpca(PROMISE, nPcs = 2, maxSteps = 2 * prod(dim(PROMISE)), unitsPerLayer = NULL, functionsPerLayer = NULL, weightDecay = 0.001, weights = NULL, verbose = interactive())

# 4. LLE (Taking huge amount of time)
d4LLE <- lle(PROMISE, m = 2, k = 10, reg = 2, ss = FALSE, p=0.5, id = FALSE, nnk = TRUE, eps = 1, iLLE = FALSE, v = 0.99)

# 5. t-SNE
## Executing the t-SNE algorithm on curated data
tsne <- Rtsne(PROMISE[-1], dims = 3, perplexity=500, max_iter = 3000, check_duplicates = FALSE)
summary(tsne$Y)

# To know system execution time to perfome t-SNE
exeTimeTsne <- system.time(Rtsne(PROMISE[-1], dims = 3, perplexity=50, max_iter = 3000, check_duplicates = FALSE))
exeTimeTsne

## Plotting
jpeg("t-SNE_p500_it3000_D3.jpg")
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=PROMISE$defective, col=colors[PROMISE$defective])
dev.off()
summary(tsne$Y)

# 6. Isomap
# Try to make it work
Isomap_Large <- Isomap(PROMISE[,c(2:21)], dims = 2, 10, mod = FALSE, plotResuduals = TRUE, verbose = TRUE)

# 15. Classical Multi Dimensional Scaling (Asking memory for a vector of size 1.3Gb)
distances <- dist(PROMISE[,c(2:21)])
d15.1_cmds <- cmdscale(distances, eig = TRUE, k = 2) # k = #of dimensions
d15.2_isomds <- isoMDS(distances, k = 2)

# Feature Reduction Methods
# 1. Boruta
str(trainLarge)

boruta.train <- Boruta(trainLarge$defective ~., data = trainLarge[,c(2:21)], doTrace = 2)
print(boruta.train)

# Ploting
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las = 2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)

# 2. Relief
ReliefWeights <- relief(trainLarge$defective ~., data = trainLarge[,c(2:21)], neighbours.count = 5, sample.size = 20)

# 3. Other Functions
# Using Logistic Regression to calculate variable importance
glmFit <- glm(trainLarge$defective, trainLarge[,c(2:21)], family = "binomial")

# Classification Algorithms:
# Categories: I) Statistical Classifiers
#            II) Nearest Neighbour Methods
#           III) Neural Networks
#            IV) SVM Based Classifiers
#             V) Decision Tree Approaches
#            VI) Ensemble Methods
#   I.1) LDA
#   I.2) QDA
#   I.3) Logistic Regression (LogReg)
#   I.4) Naive Bayes (NB)
#   I.5) Bayesian Networks (BayesNet)
#   I.6) Least Angle Regression (LARS)
#   I.7) Relevance Vector Machine (RVM)

#  II.1) K-Nearest Neighbour (K-NN)
#  II.2) K-Star (K*)

# III.1) MLP (Try Deep Neural Networks)
# III.2) RBF Netwroks (RBFNet)

#  IV.1) SVM
#  IV.2) Lagrangian SVM (L-SVM)
#  IV.3) Least Squares SVM (LS-SVM)
#  IV.4) Linear Programming (LP)
#  IV.5) Voted Perceptron (VP)

#   V.1) C 4.5 Decision Tree (C 4.5)
#   V.2) Classification and Regression Tree (CART)
#   V.3) Alternating Decision Tree (ADT)

#  VI.1) Random Forest (RandFor)
#  VI.2) Logistic Model Tree (LMT)

# 1.1 SVM Classification using t-SNE on Large Data with 3 dimensions.
tSNEScores <- tsne$Y
summary(tSNEScores)
DepVar <- PROMISE$defective
tSNEData <- data.frame(tSNEScores, DepVar <- as.factor(DepVar))

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
print(SVMtSNELarge0)
print(SVMtSNELarge1)
print(SVMtSNEpolynomial0)
print(SVMtSNEpolynomial1)

# 1.2 SVM Classification using PCA on Large Data with 3 dimensions.

SVMPCALarge1 <- svm(DepVar ~ ., data = ScoresData, kernel = "linear", cost = 10, scale = TRUE)
SVMPCALargeRadial1 <- svm(DepVar ~ ., data = ScoresData, kernel = "radial", cost = 10, scale = TRUE)
SVMPCALargeSigmoid1<- svm(DepVar ~ ., data = ScoresData, kernel = "sigmoid", cost = 10, scale = TRUE)
SVMPCALargePolynomial1 <- svm(DepVar ~ ., data = ScoresData, kernel = "polynomial", cost = 10, scale = TRUE)
SVMPCALarge0 <- svm(DepVar ~ ., data = ScoresData, kernel = "linear", cost = 10, scale = FALSE)
SVMPCALargeRadial0 <- svm(DepVar ~ ., data = ScoresData, kernel = "radial", cost = 10, scale = FALSE)
SVMPCALargeSigmoid0 <- svm(DepVar ~ ., data = ScoresData, kernel = "sigmoid", cost = 10, scale = FALSE)
SVMPCALargePolynomial0 <- svm(DepVar ~ ., data = ScoresData, kernel = "polynomial", cost = 10, scale = FALSE)

print(SVMPCALarge1)
print(SVMPCALargeRadial1)
print(SVMPCALargeSigmoid1)
print(SVMPCALargePolynomial1)
print(SVMPCALarge0)
print(SVMPCALargeRadial0)
print(SVMPCALargeSigmoid0)
print(SVMPCALargePolynomial0)


# 1.3 SVM Classification without Dimensionality reduction
SVMLarge <- svm(DepVar ~ ., data = PROMISE[,c(2:21)], kernel = "linear", cost = 10, scale = FALSE)
summary(SVMLarge)
print(SVMLarge)

plot(SVMtSNELarge1, tSNEData, DepVar~.)
# Prediction using SVM
predict(SVMPCALarge1, testData)

# Logistic Regression Process
str(tSNEData)
tSNEData$X1 <- as.factor(tSNEData$X1)
tSNEData$X2 <- as.factor(tSNEData$X2)
tSNEData$X3 <- as.factor(tSNEData$X3)
 
# Partition data training - 80%, test - 20%
set.seed(1994)
ind <- sample(2, nrow(PROMISE), replace = TRUE, prob = c(0.8,0.2))
trainLarge <- PROMISE[ind == 1,]
testLarge <- PROMISE[ind == 2,]

# 2.1 Logistic Regression using t-SNE
LRtSNELarge <- glm(DepVar ~ ., data <- tSNEData, family = binomial(link <- "logit"))
LRtSNELarge
summary(LRtSNELarge)

# 2.2 Logistic Regression using PCA
LRPCALarge <- glm(DepVar ~ ., data <- ScoresData, family = binomial(link <- "logit"))
LRPCALarge

# 2.3 Logistic Regression without Dimensionality Reduction
LRLarge <- glm(PROMISE$defective ~ ., data <- PROMISE[,c(2:21)], family = binomial(link <- "logit"))
LRLarge

# Experiments on small set of data
trainData <- read.csv(file = "4.csv", header = TRUE, sep = ",")
testData <- read.csv(file = "3.csv", header = TRUE, sep = ",")
trainData$bug
testData$bug

colors = rainbow(length(unique(trainData$bug)))
names(colors) = unique(trainData$bug)
names(colors)

defective <- replicate(nrow(trainData),0)
for (i in 1:nrow(trainData)) {
  if (trainData$bug[i] >= 1)
  {
    defective[i] = 1
  }
  else
  {
    defective[i] = 0
  }
}

trainData = trainData[c(-length(trainData))]
trainData = cbind(trainData,defective)
trainData$defective

# SVM Classification before PCA
x <- trainData[c(-length(trainData))]
x
summary(x)
y <- trainData$defective
dat <- data.frame(x, y <- as.factor(y))

svmfit1 <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit1)

# Applying PCA
pca1 <- prcomp(trainData[,c(2:21)])
summary(pca1)

pca2 <- princomp(trainData[,c(2:21)],scores = TRUE)
summary(pca2)

# Plotting PCA
autoplot(pca1, data = pca1, Label = TRUE, loadings = TRUE)
autoplot(pca2, data = pca2)

# SVM Classification uaing PCA
x <- pca1$scores[,c(1,2,3)]
x
summary(x)
x <- trainData[c(-length(trainData))]
y <- trainData$defective
dat <- data.frame(x, y)# <- as.factor(y))

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
summary(svmfit)
predict(svmfit,x)

pred <- fitted(svmfit)
pred1 <- fitted(svmfit1)
summary(pred1)
summary(pred)
y3 <- as.factor(y)
summary(y)

# Plotting SVM
plot(svmfit,as.data.frame(x))

# 3. Classical MDS
d <- dist(trainData[,c(2:21)])
scaled_2 <- cmdscale(d, k=9)
head(scaled_2)

# 4. Sammon Mapping
sam <- sammon(d = 3,y = scaled_2,k = )

# 5. Locally Linear Embedding (LLE)
trainData.lle <- lle(data <- trainLarge[,c(2:21)], k <- 50, dim <- 3)
trainData.lle
summary(trainData.lle)