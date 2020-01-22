## calling the installed package
library(Rtsne)
library(ggplot2)
library(scatterplot3d)
# Data collection: Source -- PROMISE files
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE/')
trainData <- read.csv(file = "34.csv", header = TRUE, sep = ",")

names(trainData) # Total 22 variables are present in the dataset with the name Vi(i:1->22)

## Curating the database for analysis with both t-SNE and PCA
Labels<-trainData$bug
trainData$bug<-as.factor(trainData$bug)
trainData$bug

# # Labels<-train$label
# # train$label<-as.factor(train$label)
# nrow(PROMISE)
# # Getting to know whether the module is defective or not
# PROMISE$V22 <- as.numeric(PROMISE$V22)
# PROMISE$V22 <- PROMISE$V22-1
# defective <- replicate(nrow(PROMISE),0)
# for (i in 1:nrow(PROMISE)) {
#   if (PROMISE$V22[i] >= 2)
#   {
#     defective[i] = 1
#   }
#   else
#   {
#     defective[i] = 0
#   }
# }
# 
# PROMISE = PROMISE[c(-length(PROMISE))]
# PROMISE = cbind(PROMISE,defective)
# PROMISE$defective
# write.csv(PROMISE,"PROMISE.csv", row.names=FALSE)

# Arrangement for plotting
# For each level (#bugs), assign one color to each category. 
colors = rainbow(length(unique(trainData$bug)))
names(colors) = unique(trainData$bug)
names(colors)

## Executing the algorithm on curated data
tsne_small <- Rtsne(trainData[-1], dims = 2, perplexity=50, max_iter = 10000, check_duplicates = FALSE)
summary(tsne_small$Y)
# To know system execution time to perfome t-SNE
exeTimeTsne <- system.time(Rtsne(trainData[-1], dims = 2, perplexity=50, max_iter = 5000, check_duplicates = FALSE))

exeTimeTsne

## Plotting
jpeg("t-SNE_small_p50_it10000_D2_f34.jpg")
plot(tsne_small$Y, t='n', main="t-SNE on file 34.csv")
text(tsne_small$Y, labels=trainData$bug, col=colors[trainData$bug])
dev.off()
tsne$Y.
var(tsne$Y)
# 3D plotting
scatterplot3d(as.numeric(tsne$Y), pch = 16, theta = 20, phi = 20, main = "t-SNE")
ggplot(tsne$Y, aes(x=var(tsne$Y), y=Sepal.Width, z=Petal.Length, col=colors[PROMISE$V22])) + 
  theme_void() +
  axes_3D() +
  stat_3D()
