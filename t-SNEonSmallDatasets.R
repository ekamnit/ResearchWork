# Calling the installed package
library(Rtsne)
library(ggplot2)
library(scatterplot3d)
library(plot3D)

# Data collection: Source -- PROMISE files
setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE/')
trainData <- read.csv(file = "4.csv", header = TRUE, sep = ",")
summary(trainData)
names(trainData)

# Curating the database for analysis with both t-SNE and PCA
Labels<-trainData$bug
trainData$bug<-as.factor(trainData$bug)
trainData$bug

# Arrangement for plotting
# For each level (#bugs), assign one color to each category. 
colors = rainbow(length(unique(trainData$bug)))
names(colors) = unique(trainData$bug)
names(colors)

# Executing the algorithm on curated data
tsne_small <- Rtsne(trainData[,c(-1,-22)], dims = 3, perplexity=50, max_iter = 5000, check_duplicates = FALSE)
summary(tsne_small$Y)
# To know system execution time to perfome t-SNE
exeTimeTsne <- system.time(Rtsne(trainData[-1], dims = 3, perplexity=50, max_iter = 5000, check_duplicates = FALSE))
exeTimeTsne

# Plotting
jpeg("t-SNE_small_p50_it5000_D3_f4.jpg")
plot(tsne_small$Y, t='n', main="t-SNE on file 34.csv")
text(tsne_small$Y[,1], tsne_small$Y[,2], labels=trainData$bug, col=colors[trainData$bug], pch = 20)
dev.off()

# 3D Plotting
jpeg("t-SNE_small_p50_it5000_D3_f34.jpg")
scatter3D(tsne_small$Y[,1], tsne_small$Y[,2], tsne_small$Y[,3], clab = colors, pch = 20)
dev.off()
summary(tsne_small)
