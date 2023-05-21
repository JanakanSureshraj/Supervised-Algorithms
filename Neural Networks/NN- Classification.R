#Goal: Classify the type of wine out of 3 classes based on 13 attributes.
#NN Architecture: 2 hidden layers. tanh activation. Nonlinear output. 

wine<- read.csv("C:\\IIT Computer Science\\Year 2\\Sem 2 (Jan 2023)\\2 Machine Learning and Data Mining\\Learning Resources\\Week 8 Association Rules + NN Tutorial\\Tutorial 7- Neural Networks\\winedata.csv")
head(wine)
str(wine)

#randomizing samples since they are ordered by class
#E.g.- 
v<- 1:178
v<- sample(v)
#actual application
wine_random<- wine[sample(1:nrow(wine)), ]
head(wine_random)
str(wine_random)
summary(wine_random)

#scatter plot for the data spread with 2 attributes
library(ggvis)
wine_random %>% ggvis(~Alcohol, ~Malic, fill= ~factor(Type))%>% 
  layer_points()

#normalization 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
norm_wine<- as.data.frame(lapply(wine_random[1:14], normalize))
summary(norm_wine)
summary(norm_wine$Alcohol)

#train-test split
trainset<- norm_wine[1:100, ]
testset<- norm_wine[101:178, ]

library(neuralnet)
nn <- neuralnet(Type ~ Alcohol + Malic + Ash + Alcalinity + Magnesium + Phenols + Flavanoids + Nonflavanoids + Proanthocyanins + Color + Hue + Dilution + Proline, 
                data=trainset, hidden=c(8,4), act.fct= "tanh", linear.output=FALSE)

nn$result.matrix
plot(nn)

#Testing Accuracy

#Type col needs to be removed
temp_test<- subset(testset, select=c("Alcohol", "Malic", "Ash", "Alcalinity", 
                   "Magnesium", "Phenols", "Flavanoids", "Nonflavanoids", 
                   "Proanthocyanins", "Color", "Hue", "Dilution", "Proline"))
head(temp_test)
 
#predicted results from the trained model
model_results<- compute(nn, temp_test)
predicted_type<- model_results$net.result

#de-normalizing data

#desired outputs of training and testing data 
wine_train_original_Type<- wine_random[1:100, "Type"]
wine_test_original_Type<- wine_random[101:178, "Type"]

#finding max and min values 
Type_min<- min(wine_train_original_Type)
Type_max<- max(wine_train_original_Type)

unnormalize<- function(x, min, max){
  return ((max-min)*x+min)
}
type_pred<- unnormalize(predicted_type, Type_min, Type_max)
type_pred #NN's output denormalized to original ranges 

#rounding off values
round_pred<- sapply(type_pred, round, digits=0)

final_results<- cbind(wine_test_original_Type, round_pred)
final_results

#confusion matrix
table(wine_test_original_Type, round_pred)
  
#further performance evaluation 
library(caret)
desired<- as.factor(wine_test_original_Type)
predicted<- as.factor(round_pred)
confusionMatrix(desired, predicted)

