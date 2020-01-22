set.seed(101) # Set Seed so that same sample can be reproduced in future also

# Now Selecting 50% of data as sample from total 'n' rows of the data
sample <- sample.int(n = nrow(data), size = floor(.50*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

# 1. LOOCV
score = list()

LOOCV_function = function(x,label){
  for(i in 1:nrow(x)){
    training = x[-i,]
    model = #... train model on training
      validation = x[i,]
    pred = predict(model, validation[,setdiff(names(validation),label)])
    score[[i]] = rmse(pred, validation[[label]]) # score/error of ith fold
  }
  return(unlist(score)) # returns a vector
}

# 2. K-fold CV
data(iris)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# Summarise Results
print(model)

# 3. Stratified k-fold cross validation
# Folds are created on the basis of target variable
folds <- createFolds(factor(data$target), k = 10, list = FALSE)
