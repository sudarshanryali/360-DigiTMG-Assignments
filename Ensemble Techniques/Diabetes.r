###############Start of code##########
df=file.choose()

View(df)

#Accuracy with single model
library(caret)
inTraininglocal <- createDataPartition(df$ Diabetes pedigree function, p = 0.75, list = F)
training <- df[inTraininglocal, ]
testing <- df[-inTraininglocal, ]

#install.packages("C50")
library(C50)
model <- C5.0(training$ Diabetes pedigree function ~ ., data = training[, -5])
plot(model)
pred <- predict.C5.0(model, testing[, -5])
a <- table(testing$ Diabetes pedigree function, pred)

sum(diag(a))/sum(a)

#*****************************************************************
 ########Bagging
acc <- c()
for(i in 1:11)
{
  inTraininglocal <- createDataPartition(df$ Diabetes pedigree function, p = 0.75, list = F)
  training1 <- df[inTraininglocal, ]
  testing <-df[-inTraininglocal, ]
  fittree <- C5.0(training1$ Diabetes pedigree function ~ ., data = training1[, -5])
  pred <- predict.C5.0(fittree,testing[ , -5])
  a <- table(testing$ Diabetes pedigree function, pred)
  acc <- c(acc,sum(diag(a))/sum(a))
}
acc
mean(acc)

#**************************************************************
############## Boosting

# Accuracy with single model with Boosting

inTraininglocal <- createDataPartition(df$ Diabetes pedigree function, p = 0.75, list = F)
training <- df[inTraininglocal, ]
testing <- df[-inTraininglocal, ]

model <- C5.0(training$ Diabetes pedigree function ~ ., data = training[, -5], trials = 10)
pred <- predict.C5.0(model, testing[, -5])
a <- table(testing$ Diabetes pedigree function, pred)

sum(diag(a))/sum(a)

#***************************************************************
######### Bagging and Boosting
 acc <- c()
for(i in 1:11)
{
  
  inTraininglocal <- createDataPartition(df$ Diabetes pedigree function, p = 0.75, list = F)
  training1 <- df[inTraininglocal, ]
  testing <- df[-inTraininglocal, ]
  
  fittree <- C5.0(training1$ Diabetes pedigree function ~ ., data = training1, trials = 10)
  pred <- predict.C5.0(fittree, testing[, -5])
  a <- table(testing$ Diabetes pedigree function, pred)
  
  acc <- c(acc, sum(diag(a))/sum(a))
  
}

acc
mean(acc)
