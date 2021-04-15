dataset <- read.csv("C:/Users/DELL/Multiple  Linear Regression/50_Startups.csv")

dataset$State = factor(dataset$State, 
                       levels = c('New York', 'California', 'Florida'), 
                       labels = c(1, 2, 3))
dataset$State
# Splitting the dataset into the Training set and Test set
library(glmnet)
alpha0.fit<-cv.glmnet(xtrain,ytrain,type.measure-'mse',alpha=0,family="gausian")

apha0.predicted<-predict(alpha0.fit,s=alpha0.fit$lambda.lse,newx-xtest)

predict(ytest-alpha0.fit$lambda.lse,newx-xtest)

mean((y.test-alpha0.predicted)^2)

alpha.fit<-cv.glmnet(xtrain,ytrain,type-measure='mse',alpha=1,family='gaussian')
alpha1.predicted<-predict(alpha1.fit,s=alpha1.fit $ lambda.lse, newx=x.test)

