
# Perform clustering K means clustering for the airlines data to obtain optimum number of clusters.

import pandas as pd
import numpy as np
import matplotlib.pylab as plt

from sklearn.cluster import	KMeans
# from scipy.spatial.distance import cdist 

# Loading the dataset
airlines_data = pd.read_csv("C:\\Users\\HP\\Desktop\\360Digitmg Weekly Assignments\\4) Hierarchical Clustering\\Assignment\\airlines data\\airlines.csv")
airlines_data.describe()
a_data = airlines_data.drop(["ID#"], axis = 1)

# Normalization function
def norm_func(i):
    x = (i - i.min())	/ (i.max() - i.min())
    return (x)

df_norm = norm_func(a_data)
df_norm.describe()


TWSS = []
k = list(range(2, 7))

for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(df_norm)
    TWSS.append(kmeans.inertia_)
    
TWSS

# Scree Plot or Elbow curve
plt.plot(k, TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")

## Threfore, optimal number of clusters = 4

model = KMeans(n_clusters = 4)
model.fit(df_norm)

model.labels_ # getting the labels of clusters assigned to each row 
mb = pd.Series(model.labels_)  # converting numpy array into pandas series object 
a_data['clust'] = mb # creating a  new column and assigning it to new column 

a_data.head()
df_norm.head()

a_data = a_data.iloc[:,[11,0,1,2,3,4,5,6,7,8,9,10]]
a_data.head()

a_data.iloc[:, 1:12].groupby(a_data.clust).mean()

a_data.to_csv("Kmeans_airlines.csv", encoding = "utf-8")

import os
os.getcwd()
