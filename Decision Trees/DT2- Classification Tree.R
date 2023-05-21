#Goal: Type of Wine
#Model: Classification Decision Tree 
#Data: Wine Dataset

library(rpart)
library(caret)
library(rpart.plot)

wine<- read.csv("C:\\Users\\ACER\\Downloads\\winedata.csv")
head(wine)
str(wine)

v<- 1:178
v<- sample(v)
wine_random<- wine[sample(1:nrow(wine)), ] #randomizing initial dataset

head(wine_random)
str(wine_random)
summary(wine_random)

#train-test split 
wine.scale <- cbind(wine_random[1], scale(wine_random[-1])) 
indexes = createDataPartition(wine.scale$Type, p = .7, list = F)
train = wine.scale[indexes, ]
test = wine.scale[-indexes, ]

train_x= train[, -1]
train_y= train[, 1]
test_x= test[, -1]
test_y= test[, 1]

#fit data
#method="anova" for reg. tree 
res = rpart(train_y~., data = data.frame(train_x, train_y), method = "class")
rpart.plot(res)

summary(res)

plotcp(res)
printcp(res)

#identify the optimal Complexity Parameter (CP)
cp_best <- res$cptable[which.min(res$cptable[,"xerror"]),"CP"]
cp_best

#pruning process
tree1<- prune(res, cp=cp_best)
rpart.plot(tree1)

#evaluatoin on the training subset
type_pred<- predict(object=tree1, 
                    newdata = test_x, 
                    type="class")

type_pred_num<- as.numeric(type_pred)
table(test_y, type_pred_num)

#accuracy measure/s
accuracy<- function(actual, predicted){
  mean(actual== predicted)
}
accuracy(predicted=type_pred_num, test_y)

Metrics::ce(actual=test_y, predicted= type_pred_num)

desired<- as.factor(test_y)
predicted<- type_pred
table(desired, predicted)

confusionMatrix(desired, predicted)

#rpart uses Gini method for splitting the tree. 
#Accuracy is 73.58%. "information" will be the splitting criterion now to see if there's any improvement. 
res1 = rpart(train_y~., data = data.frame(train_x, train_y), method = "class",
             parms = list(split = "information") )
rpart.plot(res1)
plotcp(res1)
printcp(res1)

#optimal CP
cp_best <- res1$cptable[which.min(res1$cptable[,"xerror"]),"CP"]
cp_best

tree2 <- prune(res1,cp = cp_best)
rpart.plot(tree2)

info_pred <- predict(object = tree2, 
                     newdata = test_x,
                     type = "class") 
Metrics::ce(actual = test_y, predicted = info_pred)

desired <-as.factor(test_y)
predicted <-info_pred
table(desired, predicted)

confusionMatrix(desired, predicted)

#Same level of performance. Accuracy is not the same always.