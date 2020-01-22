library(car)
install.packages('rattle')
data(wine, package='rattle')
attach(wine)
head(wine)
length(wine)
str(wine)
pairs.panels(wine[1:14], gap = 0, bg = c("red","green","blue")[wine$Type], pch = 21)
wine.lda <- lda(Type ~ ., data=wine)
wine.lda
# Prediction
wine.lda.values <- predict(wine.lda)
ldahist(data = wine.lda.values$x[,1], g=Type)
ldahist(data = wine.lda.values$x[,2], g=Type)
plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],Type,cex=0.7,pos=4,col="red") # add labels