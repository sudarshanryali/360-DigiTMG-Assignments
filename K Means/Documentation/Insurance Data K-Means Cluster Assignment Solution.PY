# Analyze the information given in the following ‘Insurance Policy dataset’ to create clusters of persons falling in the same type.

import pandas as pd
import numpy as np
import matplotlib.pylab as plt

from sklearn.cluster import	KMeans
# from scipy.spatial.distance import cdist 

# Loading the dataset
insurance_data = pd.read_csv("C:\\Users\\HP\\Desktop\\360Digitmg Weekly Assignments\\5) K-Means Clustering\\Assignments\\insurance data\\insurance.csv")
insurance_data.describe()

# Normalization function
def norm_func(i):
    x = (i - i.min())	/ (i.max() - i.min())
    return (x)

df_norm = norm_func(insurance_data)
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
insurance_data['clust'] = mb # creating a  new column and assigning it to new column 

insurance_data.head()
df_norm.head()

insurance_data = insurance_data.iloc[:,[5,0,1,2,3,4]]
insurance_data.head()

insurance_data.iloc[:, 1:6].groupby(insurance_data.clust).mean()

insurance_data.to_csv("Kmeans_insurance.csv", encoding = "utf-8")

import os
os.getcwd()
