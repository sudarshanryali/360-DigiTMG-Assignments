
### Perform K-Means Clustering for the crime data and identify the number of clusters formed and draw inferences.

import pandas as pd
import numpy as np
import matplotlib.pylab as plt

from sklearn.cluster import	KMeans
# from scipy.spatial.distance import cdist 

# Loading the dataset
crime_data = pd.read_csv("C:\\Users\\HP\\Desktop\\360Digitmg Weekly Assignments\\5) K-Means Clustering\\Assignments\\crime data\\crime_data.csv")
crime_data.describe()
c_data = crime_data.drop(["Unnamed: 0"], axis = 1)

# Normalization function
def norm_func(i):
    x = (i - i.min())	/ (i.max() - i.min())
    return (x)

df_norm = norm_func(c_data)
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
c_data['clust'] = mb # creating a  new column and assigning it to new column 

c_data.head()
df_norm.head()

c_data = c_data.iloc[:,[4,0,1,2,3]]
c_data.head()

c_data.iloc[:, 1:5].groupby(c_data.clust).mean()

c_data.to_csv("Kmeans_crime.csv", encoding = "utf-8")

import os
os.getcwd()
