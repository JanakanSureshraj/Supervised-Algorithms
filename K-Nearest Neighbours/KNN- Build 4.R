#Goal: Similar to KNN- Build 3 but with Cross-Validation and standardization using caret's preProcess(). 
#Dataset: wine data of wines grown in a specific area of Italy. (3 classes and 13 chemical analyses).

library(caret)

dataurl<- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
download.file(url= dataurl, destfile= "wine.data")
wine_df<- read.csv("wine.data", header= FALSE)

str(wine_df)

#Data Slicing- train test split 
intrain<- createDataPartition(y= wine_df$V1, p= 0.7, list= FALSE) #y parameter is the value of the variable to which data needs to be partitioned, 0.7= % of split 70:30 and list= return a list or matrix. 
training <- wine_df[intrain,]
testing <- wine_df[-intrain,]
dim(training)
dim(testing)

#Preprocessing 
anyNA(wine_df) #check for missing values 
summary(wine_df)
#converting V1 values (1,2,3 numbers) to factors (categorical variables)
training[["V1"]] = factor(training[["V1"]])

#KNN Model 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#method= repeated cross validation,  number= resampling iterations, repeats= complete sets of folds to compute for the repeated cross-validation
#trainControl() returns a list. This list will be passed to train(). 
set.seed(3333)
knn_fit <- train(V1 ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
               tuneLength = 10)
#V1~ means using all attributes and V1 as the target variable, method= knn, trControl= results from trainControl, preProcess= pre-processing (madatory), tuneLength= integer value for tuning the algorithm. 

#Result
knn_fit
#plot
ggplot(data = knn_fit, aes(x = 1:23, y = knn_fit$results$Accuracy)) +
  geom_line(color = "Blue")
#Accuracy is highest when k= 17 and that k is chosen automatically. 

#test set pred 
test_pred<- predict(knn_fit, newdata = testing)
test_pred

#Confusion Matrix
desired_test= factor(testing$V1) #making the class values to categorical variables or factors 
confusionMatrix(test_pred, desired_test)
