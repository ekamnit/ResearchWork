# Feature Selection Algorithms
# 1. Boruta
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE/')
trainData <- read.csv(file = "2.csv", header = TRUE, sep = ",")
trainData <- trainData[-1]
names(trainData)
set.seed(1607)
boruta.train <- Boruta(trainData$bug~., data = trainData, doTrace = 2)
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
class(boruta.df)
print(boruta.df)

# 2. Relief
ReliefWeights <- relief(trainLarge$defective ~., data = trainLarge[,c(2:21)], neighbours.count = 5, sample.size = 20)

# 3 Variable Importance

# 3.1 Using Logistic Regression
glmFit <- glm(trainLarge$defective~., trainLarge[,c(2:21)], family = "binomial")
summary(glmFit)
varImp(glmFit)

# 3.2 Using Random Forest
rfFit <- randomForest(trainLarge$defective~., data = trainLarge[,c(2:21)], importance = TRUE)

# Create an importance based on mean decreasing gini
importance(rfFit)
varImp(rfFit)

# Create a plot of importance scores by random forest
varImpPlot(rfFit)

