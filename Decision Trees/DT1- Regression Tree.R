#Goal: House Price Prediction 
#Model: Regression Decision Tree 
#Data: Boston Housing Data in the MASS package 

library(rpart)
library(rpart.plot)
library(caret)

boston= MASS::Boston
str(boston)

#train-test split
indexes<-createDataPartition(boston$medv, p=.80, list=F)
boston_norm <- cbind(scale(boston[-14]), boston[14]) #scaling input variables due to the nature of the data
summary(boston_norm)

train= boston_norm[indexes, ]
test= boston_norm[-indexes,]

train_x<- train[, -14] #input variables in the training set 
train_y<- train[, 14] #desired output in the training set 
test_x<- test[, -14] 
test_y<- test[, 14]

#fit the training data
#Complexity Parameter (CP) controls the complexity of the tree. 
#Smaller the CP value, the larger the tree rpart will attempt to fit. 
#Default value of CP= 0.01. 
#Role of this parameter is to prune off the splits that are not worthwhile to save computing time. 
fit<- rpart(train_y~., data=data.frame(train_x, train_y), method="anova"
            , control=list(cp=0.01) )
#visualize the model
rpart.plot(fit, roundint=F, digits=3)

prp(fit, extra=1, roundint=T, digits=3)
plotcp(fit)
printcp(fit)

cp_best<- fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"]
cp_best
fit.pruned<- prune(fit, cp= cp_best)
rpart.plot(fit.pruned, roundint=F, digits=3)

#prediction
pred_y<- predict(fit.pruned, data.frame(test_x))
print(data.frame(test_y, pred_y))

#accuracy measure/s
mse<- mean((test_y-pred_y)^2)
mae<-  caret::MAE(test_y, pred_y)
rmse<- caret::RMSE(test_y, pred_y)
cat("MSE: ", mse, "MAE:", mae, "RMSE: ", rmse)

#style graph for the results
x=1:length(test_y)
plot(x, test_y, col="red", lwd=2, main="Boston housing test data prediction")
lines(x, pred_y, col="blue", lwd=2)
legend("topright", legend= c("original-medv", "predicted-medv"), 
       fill= c("red", "blue"), col=2:3, adj= c(0, 0.6))
grid()

#minsplit option= minimum number of data points required to attempt a split before it is forced to create a terminal node
#default minsplit=20, reducing it to 10-> terminal nodes with smaller observations can create a predicted value
fit_new= rpart(train_y~., data=data.frame(train_x, train_y), 
               control= list(cp=0.01, minsplit=10), 
               method="anova")
#The above procedure can be repeated to define the optimal CP. 
#CP controls the size and performance of the DT. 