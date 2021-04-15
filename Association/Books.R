# Prepare rules for the dataset "Books.csv"
# 1) Try different values of support and confidence. Observe the change in number of rules for different support, confidence values
# 2) Change the minimum length in apriori algorithm
# 3) Visualize the obtained rules using different plots


# install.packages("arules")
# install.packages("igraph")

library(arules)   
library("arulesViz")
library(readr)

# Loading the datset
Books=read.csv(choose.files(),colClasses = "factor")

# EDA
Books[1:5]     
class(Books)    
str(Books)      
summary(Books)

# Building rules using apriori algorithm
arules <- apriori(Books, parameter = list(support = 0.002, confidence = 0.75, minlen = 2))
arules

inspect(head(sort(arules, by = "lift"))) 

head(quality(arules))


# To get the visualization

windows()
plot(arules)
plot(arules, method = "grouped")


plot(arules[1:10], method = "graph")


write(arules, file = "a_rules_books.csv", sep = ",")
getwd()

#S= 0.011 C=0.0.80
arules_1 <- apriori(Books, parameter = list(support = 0.011, confidence = 0.80, minlen = 2))
arules_1

inspect(head(sort(arules_1, by = "lift"))) 

head(quality(arules_1))

windows()
plot(arules_1)
plot(arules, method = "grouped")
plot(arules_1[1:10], method = "graph")


#S= 0.0001 C=0.90 minlen=3
arules2 <- apriori(Books, parameter = list(support = 0.0001, confidence = 0.90, minlen = 3))
arules2

inspect(head(sort(arules2, by = "lift"))) 
head(quality(arules2))



windows()
plot(arules2)
plot(arules2, method = "grouped")
plot(arules2[1:10], method = "graph")



#########################################
# arules = 173970
# arules1 = 69188
# arules2 = 186541