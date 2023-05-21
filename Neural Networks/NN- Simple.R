#Goal: Calculating the square root of numbers. 
#NN Architecture: 1 hidden layer. Sigmoid activation function. Linear Output. 
library(neuralnet)

training_input<- as.data.frame(runif(50, min=0, max=100))#generate 50 random numbers within 0-100
training_output<- sqrt(training_input)

#Training dataset
#column bind the data into a variable- training dataset
training_data<- cbind(training_input, training_output)
colnames(training_data)<- c("Input", "Output")

#training the NN
net.sqrt<- neuralnet(Output ~ Input, data= training_data, hidden= 6, act.fct= "logistic", err.fct="sse", linear.output= T)
net.sqrt	

#plot the NN
plot(net.sqrt)

#Testing dataset 
#generate sqr numbers
test_data<-as.data.frame((1:10)^2)
#running test data through the neural network
net_results<- compute(net.sqrt, test_data)

#Creating the desired output
test_data_result<- sqrt(test_data)
test_data_result
final_results<- cbind(test_data_result, net_results$net.result)
colnames(final_results)<- c("Test Data", "NN Result")
final_results

#Performance evaluation
library(MLmetrics)
RMSE(final_results$`Test Data`, final_results$`NN Result`)

library(Metrics)
mse(final_results$`Test Data`, final_results$`NN Result`)

clean_output<- cbind(test_data, sqrt(test_data), net.results$net.result)
colnames(clean_output)<- c("Input", "Expected Output", "NN OUtput")
clean_output
