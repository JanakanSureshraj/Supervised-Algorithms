#KNN is a simple yet powerful algorithm which makes no assumptions about the underlying data distribution 
#and hence can be used in cases where relationships between features and classes are complex or difficult to understand.

#Goal: classify type of wine based on 13 chemical analyses. 
#Dataset: wine data of wines grown in a specific area of Italy.
 
library(ggvis)
library(gmodels)

wine<- read.csv("winedata.csv")
head(wine)
str(wine)

#randomizing samples 
wine_random<- wine[sample(1:nrow(wine)), ]
head(wine_random)
str(wine_random)

wine_random %>% ggvis(~ Alcohol, ~Malic, fill= ~factor(Type)) %>% layer_points()

#features need to be in the same scale 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
norm_wine <- as.data.frame(lapply(wine_random[2:14], normalize))  
summary(norm_wine)
summary(norm_wine$Alcohol)

#train-test split 
trainset <- norm_wine[1:100, ]
testset <- norm_wine[101:178, ]

#labels
trainset_labels <- wine_random[1:100, 1]
testset_labels <- wine_random[101:178, 1]

#KNN Model 
knn_prediction <- knn(train = trainset, test = testset,cl = trainset_labels, k=15) #cl= A vector containing the class labels. Also called as the factor vector.

#Accuracy
CrossTable(x=testset_labels, y=knn_prediction,prop.chisq=FALSE)
