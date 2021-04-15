# Analyze the information given in the following 'Insurance Policy dataset' to create clusters of persons falling in the same type.



library(plyr)
library(readr)
library(animation)

insurance_data <- read_csv(file.choose())

normalized_data <- scale(insurance_data)
normalized_data


twss <- NULL
for (i in 2:8) {
  twss <- c(twss, kmeans(normalized_data, centers = i)$tot.withinss)
}
twss


plot(2:8, twss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

# From Scree Plot we see optimal k=4

fit_data <- kmeans(normalized_data, 4) 

fit_data$cluster
final_data <- data.frame(fit_data$cluster, insurance_data) # Append cluster membership

aggregate(insurance_data, by = list(fit_data$cluster), FUN = mean)

write_csv(final_data, "assignment_insurance_data_kmeans.csv")
getwd()



