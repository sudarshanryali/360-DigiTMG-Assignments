# Importing the dataset 
dataset<-read.csv("C:/Users/DELL/Desktop/Data sets/delivery_time.csv")
dataset
library(caTools)
# Splitting the dataset into the 
# Training set and Test set 

split=sample.split(dataset$Sorting_Time,SplitRatio = 0.7)
trainingset=subset(dataset,split==TRUE)
testset=subset(dataset,split==FALSE)
# Fitting Simple Linear Regression to the Training set
lm.r=lm(formula=Delivery_Time ~ Sorting_Time, data=trainingset)
coef(lm.r)
# Predicting the Test set results
ypred=predict(lm.r,newdata=testset)

library(ggplot2)
# Visualising the Training set results 
ggplot() + geom_point(aes(x = trainingset$Sorting_Time,  
                          y = trainingset$Delivery_Time), colour = 'red') +
  geom_line(aes(x = trainingset$Sorting_Time, 
                y = predict(lm.r, newdata = trainingset)), colour = 'blue')
# Visualising the Test set results 

ggplot() + geom_point(aes(x = testset$Sorting_Time,  
                          y = testset$Delivery_Time), colour = 'red') +
  geom_line(aes(x = trainingset$Sorting_Time, 
                y = predict(lm.r, newdata = trainingset)), colour = 'blue')

