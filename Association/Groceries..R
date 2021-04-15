# Prepare rules for the dataset "Groceries.csv"
# 1) Try different values of support and confidence. Observe the change in number of rules for different support, confidence values
# 2) Change the minimum length in apriori algorithm
# 3) Visualize the obtained rules using different plots


# install.packages("arules")
# install.packages("igraph")

library(arules)   
library("arulesViz")
library(readr)

# Loading the dataset
Groceries <- read.csv(file.choose(), colClasses = "factor")

# EDA
Groceries[1:4]     
class(Groceries)    
str(Groceries)      
summary(Groceries)


arules <- apriori(Groceries, parameter = list(support = 0.002, confidence = 0.75, minlen = 2))
arules

inspect(head(sort(arules, by = "lift"))) 

head(quality(arules))


# To get the visualization

windows()
plot(arules)
plot(arules, method = "grouped")


plot(arules[1:10], method = "graph")


write(arules, file = "a_rules_Groceries.csv", sep = ",")
getwd()


# S=0.001 C=0.95 

arules1 <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.95, minlen = 2))
arules1

inspect(head(sort(arules1, by = "lift"))) 

head(quality(arules1))


# To get the visualization

windows()
plot(arules1)
plot(arules1, method = "grouped")


plot(arules1[1:10], method = "graph")





#S= 0.013 C=0.60 minlen=3
arules2 <- apriori(Groceries, parameter = list(support = 0.013, confidence = 0.60, minlen = 3))
arules2

inspect(head(sort(arules2, by = "lift"))) 
head(quality(arules2))



windows()
plot(arules2)
plot(arules2, method = "grouped")
plot(arules2[1:10], method = "graph")

#########################################
# arules = 325
# arules1 =399
# arules2 = 38