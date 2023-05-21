#Goal: A Naive Bayes model to predict species from the 4 measurements in the iris dataset

install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)
data(iris)
str(iris)
summary(iris)

#train-test split
train.indices<- sample(1:nrow(iris), 100)
iris.train<- iris[train.indices, ]
iris.test<- iris[-train.indices, ]

classifier1<- naiveBayes(iris.train[, 1:4], iris.train[, 5])
#alternatively, classifier1<- naiveBayes(Species~., data=iris.train)
classifier1

#Plot of the petal length from the above outcome
plot(function(x) dnorm(x, 1.448, 0.178), 0, 8, col="red", 
     main="Petal Length Distribution")
curve(dnorm(x, 4.27, 0.393), add=TRUE, col="blue")
curve(dnorm(x, 5.454, 0.512), add=TRUE, col="green")

table(predict(classifier1, iris.test[, -5]), iris.test[, 5], 
              dnn=list("predicted", "actual"))

#Accuracy, Confusion Matrix and Statistics
accuracy_iris<- predict(classifier1, iris.test[, -5])
test_table<- table(accuracy_iris, iris.test[, 5])
test_table
test_accuracy<- sum(diag(test_table))/sum(test_table)
test_accuracy

#alternative way
test_accuracy2<- confusionMatrix(accuracy_iris, iris.test$Species)
test_accuracy2
