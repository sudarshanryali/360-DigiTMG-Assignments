####Perform K-Means Clustering for the crime data and identify the number of clusters formed and draw inferences.


library(plyr)
library(readr)
library(animation)

crime_data <- read_csv(file.choose())

data <- crime_data[ , 2:5]


normalized_data <- scale(data)
normalized_data


twss <- NULL
for (i in 2:5) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss


plot(2:5, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

# From scree plot we find, optimal k=4

fit_data <- kmeans(normalized_data, 4) 

fit_data$cluster
final_data <- data.frame(fit_data$cluster, data) # Append cluster membership

aggregate(data, by = list(fit_data$cluster), FUN = mean)

write_csv(final_data, "assignment_crime_data_kmeans.csv")
getwd()



