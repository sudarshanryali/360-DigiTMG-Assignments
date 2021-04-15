# Perform K means clustering for the airlines data to obtain optimum number of clusters.
# Draw the inferences from the clusters obtained.

library(plyr)
library(readr)
library(animation)

airlines_data <- read_csv(file.choose())

data <- airlines_data[ ,-1]

normalized_data <- scale(data)
normalized_data


twss <- NULL
for (i in 2:6) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss


plot(2:6, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

# From scree plot we find optimal k=3

fit_data <- kmeans(normalized_data, 3) 

fit_data$cluster
final_data <- data.frame(fit_data$cluster, data) # Append cluster membership

aggregate(data, by = list(fit_data$cluster), FUN = mean)

write_csv(final_data, "assignment_airlines_data_kmeans.csv")
getwd()



