dataset <- read_csv("C:/Users/DELL/Multiple Linear Regression/ToyotaCorolla.csv")
dataset
dataset$V11=factor(dataset$V11,
                   levels = c('Blue', 'Silver', 'Black', 'White', 'Grey', 'Red', 'Green','Yellow', 'Violet', 'Beige'), 
                   labels = c(1,2,3,4,5,6,7,8,9,10))
dataset$V8=factor(dataset$V8,
                  levels = c('Diesel', 'Petrol', 'CNG'), 
                  labels = c(1,2,3))
dataset

library(glmnet)
alpha0.fit<-cv.glmnet(xtrain,ytrain,type.measure-'mse',alpha=0,family="gausian")

apha0.predicted<-predict(alpha0.fit,s=alpha0.fit$lambda.lse,newx-xtest)

predict(ytest-alpha0.fit$lambda.lse,newx-xtest)

mean((y.test-alpha0.predicted)^2)

alpha.fit<-cv.glmnet(xtrain,ytrain,type-measure='mse',alpha=1,family='gaussian')
alpha1.predicted<-predict(alpha1.fit,s=alpha1.fit $ lambda.lse, newx=x.test)