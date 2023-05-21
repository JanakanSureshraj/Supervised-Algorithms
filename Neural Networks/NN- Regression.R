#Goal: Predicting the strength of cement mix based on relevant features. 
#NN Architecture: 2 hidden layers. relu activation function. Linear output (lin act.)- continous 
#                 output variable is directly proportional to the weighted 
#                 sum of the inputs (no activation).  

library(ggplot2)
library(reshape2)
library(gridExtra)
library(neuralnet)
library(grid)
library(MASS)
library(devtools)
library(neuralnet)


#Loading the dataset
concrete<- read.csv("C:\\IIT Computer Science\\Year 2\\Sem 2 (Jan 2023)\\2 Machine Learning and Data Mining\\Learning Resources\\Week 8 Association Rules + NN Tutorial\\Tutorial 7- Neural Networks\\concrete.csv")
str(concrete)
summary(concrete)
#summary of a specific variable
summary(concrete$strength)

#Scaling
#Normalization is performed column by column as each attribute haS a different value range
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#applying normalization to every column using lapply()
concrete_norm<- as.data.frame(lapply(concrete, normalize))
head(concrete_norm)
summary(concrete_norm)

#train-test split
concrete_train<- concrete_norm[1:773,]
concrete_test<- concrete_norm[774:1030, ]

#training the NN 
#NN arch.: 2 hidden layers with 8 and 8 neurons respectively
concrete_model<- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
                           hidden=c(5,3), data= concrete_train, act.fct= "relu", linear.output = TRUE)
plot(concrete_model)
#Total parameters: Bias= 13 and Weights= 108 -> 121

#predictions on the test dataset 
model_results<- compute(concrete_model, concrete_test[1:8])
predicted_strength<- model_results$net.result

#extract the original (before normalizing) training and testing desired outputs
concrete_train_original_strength<- concrete[1:773, "strength"]
concrete_test_original_strength<- concrete[774:1030, "strength"]

#finding maximum and minimum values
strength_min<- min(concrete_train_original_strength)
strength_max<- max(concrete_train_original_strength)

#display its contents
head(concrete_train_original_strength)

#creating the reverse of the norm. function 
unnormalize<- function(x, min, max){
  return ((max-min)*x+min)
}

#de-normalizing the NN's output 
strength_pred<- unnormalize(predicted_strength, strength_min, strength_max)
head(strength_pred) #NN's output for the original data ranges 

#performance indices 
#all performance indices should be made on real values and not on the normalized data
#defining RMSE function
rmse1<- function(error){
  sqrt(mean(error^2))
}
error<- (concrete_test_original_strength - strength_pred)
pred_RMSE<- rmse1(error)
pred_RMSE

#built in RMSE function 
library(Metrics)
rmse(concrete_test_original_strength, strength_pred)

#built in RMSE function- different package
library(MLmetrics)
rmse(concrete_test_original_strength, strength_pred)

#Plotting
#Line of best fit for reg. problems- closer the data to the line, better the prediction
par(mfrow= c(1,1))
plot(concrete_test_original_strength, strength_pred, col="red", 
     main="Real vs. Predicted", pch= 18, cex= 0.7)
abline(a=0, b=1, h=90, v=90)

x=1:length(concrete_test_original_strength)
plot(x, concrete_test_original_strength, col="red", type="b", lwd=2, 
     main="concrete strength prediction")
lines(x, strength_pred, col="blue", lwd="red")
legend("topright", legend=c("original-strength", "predicted strength"),
       fill=c("red", "blue"), col=2:3, adj= c(0, 0.6))

clean_output<- cbind(concrete_test_original_strength, strength_pred)
head(clean_output)

