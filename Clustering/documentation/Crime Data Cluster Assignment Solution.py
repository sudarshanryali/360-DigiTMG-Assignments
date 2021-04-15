
### Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

import pandas as pd
import matplotlib.pylab as plt

crime_data = pd.read_csv ("C:\\Users\\HP\Desktop\\360Digitmg Weekly Assignments\\4) Hierarchical Clustering\\Assignment\crime data\\crime_data.csv")

crime_data.describe()
crime_data.info()
new_cdata = crime_data.drop(["Unnamed: 0"], axis=1)


# Normalization function
 
def norm_func(i):
    x = (i-i.min())	/ (i.max()-i.min())   # Custom Function
    return (x)


# Normalized data frame (considering the numerical part of data)
df_norm = norm_func(new_cdata)
df_norm.describe()



# for creating dendrogram 
from scipy.cluster.hierarchy import linkage
import scipy.cluster.hierarchy as sch 

z = linkage(df_norm, method = "complete", metric = "euclidean")


# Dendrogram
plt.figure(figsize=(15, 8));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(z, 
    leaf_rotation = 0,  # rotates the x axis labels
    leaf_font_size = 10 # font size for the x axis labels
)
plt.show()


# Now applying AgglomerativeClustering 
from sklearn.cluster import AgglomerativeClustering

h_complete = AgglomerativeClustering(n_clusters = 3, linkage = 'complete', affinity = "euclidean").fit(df_norm) 
h_complete.labels_

cluster_labels = pd.Series(h_complete.labels_)

new_cdata['clust'] = cluster_labels # creating a new column and assigning it to new column 

new_cdata = new_cdata.iloc[:, [4,0,1,2,3]]
new_cdata.head()     # First 5 rows
 
# Aggregate mean of each cluster
new_cdata.iloc[:,:].groupby(new_cdata.clust).mean()

# creating a csv file 
new_data.to_csv("Crime.csv", encoding = "utf-8")

import os
os.getcwd()



