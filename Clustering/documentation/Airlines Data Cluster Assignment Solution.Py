
# Perform clustering for the airlines data to obtain optimum number of clusters. Draw the inferences from the clusters obtained.

import pandas as pd
import matplotlib.pylab as plt

airlines_data = pd.read_csv ("C:\\Users\\HP\Desktop\\360Digitmg Weekly Assignments\\4) Hierarchical Clustering\\Assignment\\airlines data\\airlines.csv")

airlines_data.describe()
airlines_data.info()
a_data = airlines_data.drop(["ID#"], axis=1)


# Normalization function
 
def norm_func(i):
    x = (i-i.min())	/ (i.max()-i.min())   # Custom Function
    return (x)


# Normalized data frame (considering the numerical part of data)
df_norm = norm_func(a_data)
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

h_complete = AgglomerativeClustering(n_clusters = 4, linkage = 'complete', affinity = "euclidean").fit(df_norm) 
h_complete.labels_

cluster_labels = pd.Series(h_complete.labels_)

a_data['clust'] = cluster_labels # creating a new column and assigning it to new column 

a_data = a_data.iloc[:, [11,0,1,2,3,4,5,6,7,8,9,10]]
a_data.head()     # First 5 rows
 
# Aggregate mean of each cluster
a_data.iloc[:,1:11].groupby(a_data.clust).mean()

# creating a csv file 
a_data.to_csv("Airlines.csv", encoding = "utf-8")

import os
os.getcwd()



