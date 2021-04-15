dataset<-read.csv("C:/Users/DELL/Desktop/Data sets/Computer_Data.csv")
dataset
dataset$cd = factor(dataset$cd , 
                    levels = c('no', 'yes'), 
                    labels = c(1, 2))
dataset$multi  = factor(dataset$multi  , 
                        levels = c('no', 'yes'), 
                        labels = c(1, 2))
dataset$premium  = factor(dataset$premium  , 
                          levels = c('no', 'yes'), 
                          levels = c('no', 'yes'), 
                          labels = c(1, 2))
dataset

library(glmnet)
alpha0.fit<-cv.glmnet(xtrain,ytrain,type.measure-'mse',alpha=0,family="gausian")

apha0.predicted<-predict(alpha0.fit,s=alpha0.fit$lambda.lse,newx-xtest)

predict(ytest-alpha0.fit$lambda.lse,newx-xtest)

mean((y.test-alpha0.predicted)^2)

alpha.fit<-cv.glmnet(xtrain,ytrain,type-measure='mse',alpha=1,family='gaussian')
alpha1.predicted<-predict(alpha1.fit,s=alpha1.fit $ lambda.lse, newx=x.test)

