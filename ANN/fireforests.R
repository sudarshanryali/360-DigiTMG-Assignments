library(caret)
library(grDevices)
library(leaps)
library(relaimpo)
library(corrplot)
library(car)
library(DAAG)
options(scipen = 999,digits = 3)
forestdata <- read.csv(file.choose())
origdata <- forestdata
dim(forestdata)
colnames(forestdata)
sum(is.na(forestdata))
length(which(forestdata$area==0))
forestdata$month <- as.numeric(as.factor(forestdata$month))
forestdata$day <- as.numeric(as.factor(forestdata$day))
par(mfrow=c(2,6),mar=c(3.90, 4.25, 2.5, 0.5))
for (variables in 1:(dim(forestdata)[2]-1)){
  thisvar = forestdata[,variables]
  d <- density(thisvar)
  plot(d, main = names(forestdata[variables]),xlab="")
  polygon(d, col="cyan", border="blue")
  title("Density plots for all 11 Model Variables", line = -19, outer = TRUE)}

print(paste("Percentage non-zero rain: ",round(length(which(forestdata$rain>0)) /dim(forestdata)[1]*100,2)))
forestdata <- forestdata[,-which(colnames(forestdata)== "rain")]

par(mfrow=c(1,2),mar=c(5, 4.25, 5.5, 2))
d <- density(forestdata$FFMC)
plot(d,main="FFMC Density (original)",xlab="FFMC index", col='tomato', lwd=3)
forestdata$FFMC<- (forestdata$FFMC^3)
d <- density(forestdata$FFMC)
plot(d,main="FFMC Density (x^3)",xlab="FFMC index", col='tomato', lwd=3)

par(mfrow=c(1,2),mar=c(5, 4.25, 5.5, 2))
d <- density(forestdata$area)
plot(d,main="Area Burned Density (original)",xlab="Area Burned (Hec)", col='tomato', lwd=3)
d <- density(log(forestdata$area+1))
plot(d,main="Area Burned Density (log(x+1))",xlab="Area Burned (Hec)", col='tomato', lwd=3)

forestdata$area <- log(forestdata$area+1)

par(mfrow=c(1,1))
M <- cor(forestdata)
corrplot(M, method="color", outline = TRUE,type="lower",order = "hclust",
         tl.col="black", tl.srt=45, diag=FALSE,tl.cex = 1,mar=c(0,0,3,0),
         title="Correlation Matrix between Predictor and Outcome variables")

assumptionsmodel <- lm(area ~ ., data=forestdata)
lmtest::bptest(assumptionsmodel)
par(mfrow=c(2,2))
plot(assumptionsmodel)

assumptionsmodel_all <- lm(area ~ ., data=forestdata)
assumptionsmodel_0 <- lm(area ~ .,data=forestdata[which(forestdata$area>0),])
# Remove all cases with an area burned of 0
forestdata <- forestdata[which(forestdata$area>0),]
# Plots both with and without 0 residuals
par(mfrow=c(1,2))
hist(assumptionsmodel_all$residuals, main = "Data with 0 area burned", xlab = 'Residuals')
abline(v=mean(assumptionsmodel_all$residuals), col='red', lwd=2)
hist(assumptionsmodel_0$residuals,main = "Data without 0 area burned", xlab = 'Residuals')
abline(v=mean(assumptionsmodel_0$residuals), col='red', lwd=2)

model1 <- lm(area ~ ., data=forestdata)
outcome1 <- summary(model1)
(round(outcome1$coefficients[ , c(2,4)],3))
# Find which variables are significant
sig.pred <- row.names(outcome1$coefficients)[which(outcome1$coefficients[ ,4] <= 0.05)]
rsquare <- round(summary(model1)$r.squared,3)
print(paste("The R2 value is ",rsquare))

forestdata$FFMC.DMC <- forestdata$FFMC*forestdata$DMC
forestdata$FFMC.DC <-forestdata$FFMC*forestdata$DC
forestdata$FFMC.ISI <-forestdata$FFMC*forestdata$ISI
forestdata$DMC.DC<-forestdata$DMC*forestdata$DC
forestdata$DMC.ISI<-forestdata$DMC*forestdata$ISI
forestdata$DC.ISI<-forestdata$DC*forestdata$ISI

# Create interactive terms for Weather
forestdata$wind.temp<-(forestdata$wind)*(forestdata$temp)
forestdata$temp.RH<-(forestdata$temp)*(forestdata$RH)
forestdata$wind.RH<-(forestdata$wind)*(forestdata$RH)

# Model with original and interactive terms
model2 <- lm(area~.,data=forestdata)
outcome2 <- summary(model2)
# Find which variables are significant
sig.pred <- row.names(outcome2$coefficients)[which(outcome2$coefficients[ ,4] <= 0.05)]
rsquare <- round(summary(model2)$r.squared,3)
print(paste("The R2 value is ",rsquare))

influencePlot(model2,id.n=5)

forestdata2 <- forestdata[-which(row.names(forestdata) %in% c(200,470)),]

# Run full model again with outliers removed
model3 <- lm(area~.,data=forestdata2)
outcome3 <- summary(model3)
# Find which variables are significant
sig.pred <- row.names(outcome3$coefficients)[which(outcome3$coefficients[ ,4] <= 0.05)]
rsquare <- round(summary(model3)$r.squared,3)
print(paste("The R2 value is ",rsquare))

par(mfrow=c(1,1))
cv_all<-cv.lm(data=forestdata2, model3, m=3) 

RMSD_origin<-sqrt(mean((cv_all$cvpred-cv_all$area)^2))
print(paste("All variable model RMSD: ",RMSD_origin))
## [1] "All variable model RMSD:  1.34849411888118"

subset.out<-regsubsets(area~ ., data=forestdata2,nbest=1,nvmax=NULL,method="exhaustive")
summary.out<-summary(subset.out)
# Returns the variables used in the best model
bestmodelvariables<- summary.out$which[which.max(summary.out$adjr2),]  
par(mfrow=c(1,1))
# PLot all the subsets against the adjusted r2 value
plot(subset.out,scale="adjr2",main="All Subset Regression")

variables <- names(bestmodelvariables[which(bestmodelvariables==TRUE)])[-1]

RMSD_final<-sqrt(mean((cv_final$cvpred-cv_final$area)^2))
print(paste("Subset variable model RMSD: ",round(RMSD_final,2)))

modelinput <- data.frame(matrix(nrow=1, ncol=length(origdata)-1))
colnames(modelinput) <- colnames(origdata)[-length(origdata)]
# Loop through each variable and ask ser to input the value
for (i in 1:(length(origdata)-1)){
  thisvar <- colnames(origdata)[i]
  varinput <- readline(paste("Input the ",thisvar," value:"))
  varinput <- as.numeric(unlist(strsplit(varinput, ",")))
  modelinput[1,i] <- varinput
}
# Create interactive terms for the Fire index
modelinput$FFMC.DMC <- modelinput$FFMC*modelinput$DMC
modelinput$FFMC.DC <-modelinput$FFMC*modelinput$DC
modelinput$FFMC.ISI <-modelinput$FFMC*modelinput$ISI
modelinput$DMC.DC<-modelinput$DMC*modelinput$DC
modelinput$DMC.ISI<-modelinput$DMC*modelinput$ISI
modelinput$DC.ISI<-modelinput$DC*modelinput$ISI

# Create interactive terms for Weather
modelinput$wind.temp<-(modelinput$wind)*(modelinput$temp)
modelinput$temp.RH<-(modelinput$temp)*(modelinput$RH)
modelinput$wind.RH<-(modelinput$wind)*(modelinput$RH)
# Use the model to predict new value
modelinput <- modelinput
newpred <- predict(modelfinal, modelinput)
# Backwards transformation to original hec. value.
newpred <- exp(newpred)-1
# Print out new value
print(paste("We expect fires in these conditions to burn ",round(newpred,2)," hec"))
