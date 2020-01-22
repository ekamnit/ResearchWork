## Calling the installed package
library(Rtsne)
library(ggplot2)
library(scatterplot3d)
library(stats)
library(stats4)
library(MASS)
library(lle)
library(generator)
library(e1071)
library(ggfortify)
library(ISLR)
library(rpart)
library(neuralnet)

# Data collection
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE/')
trainset <- read.csv(file = "4.csv", header = TRUE, sep = ",")
testset <- read.csv(file = "3.csv", header = TRUE, sep = ",")
trainset$bug
testset$bug
testset <- testset[-testset$bug]
class(trainset)

# Neural Netowrks implementation
n = names( trainset )
f = as.formula( paste( "class ~", paste( n[!n %in% "class"], collapse = "+" ) ) )
nn = neuralnet( f, trainset, hidden = 4, linear.output = FALSE, threshold = 0.01 )
class(f)
plot( nn, rep = "best" )
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