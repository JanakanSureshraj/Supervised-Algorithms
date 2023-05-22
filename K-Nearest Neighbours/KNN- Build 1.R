#Goal: Classify species. 
#Dataset: Iris dataset 

library(datasets)
library(ggplot2)
library(gridExtra) 
library(class)
library(caret)

head(iris)
summary(iris)

p1<-  ggplot(iris, aes(x= Sepal.Length, y= Sepal.Width
                       , col= Species))+ geom_point(alpha= 0.8)
p2<-  ggplot(iris, aes(x= Petal.Length, y= Petal.Width
                       , col= Species))+ geom_point(alpha= 0.8)
grid.arrange(p1, p2, ncol= 2)

#Building a KNN Model
ind <- sample(1:nrow(iris), nrow(iris)*0.7)     
iris.train <- iris[ind, ]
iris.test <- iris[-ind, ]

#random selection of k = 3
knn.fit<- knn(train= iris.train[, 1:4], test= iris.test[, 1:4], 
              cl= iris.train$Species, k= 3)
error<-  1-mean(knn.fit == iris.test$Species)
accuracy<- mean(knn.fit == iris.test$Species)
cat("Training Accuracy:", accuracy, sep="")

table(iris.test$Species, knn.fit)
nb.test.acc1<- confusionMatrix(iris.test$Species, knn.fit, mode= "everything")
nb.test.acc1

#plot the KNN model’s training accuracy for several values of K.
error<- c()
valid_acc<- c()

for (i in 1:19){
  knn.fit <- knn(train = iris.train[,1:4], test = iris.test[,1:4], cl = iris.train$Species, k = i)
  error[i] = 1- mean(knn.fit == iris.test$Species)
  valid_acc <- c(valid_acc, mean(knn.fit == iris.test$Species))
}
ggplot(data = data.frame(error), aes(x = 1:19, y = error)) +
  geom_line(color = "Blue")

max(valid_acc)
which.max(valid_acc) 
