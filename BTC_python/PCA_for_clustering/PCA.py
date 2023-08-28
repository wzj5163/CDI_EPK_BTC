import numpy as np
import pandas as pd
from sklearn import datasets
import matplotlib.pyplot as plt



from sklearn.decomposition import PCA



#Approach 1
import os
path="/Users/ratmir/Downloads"
os.chdir(path)



df = pd.read_csv('Q9.csv')
N = 95 # number of columns of df-1 (because first column is 'return' column)
df = df.iloc[ :, 1:N]
cols = list(df.columns)
cols.reverse()
df = df[cols] #reverse cols


import numpy as np
import scipy
from scipy.cluster.hierarchy import dendrogram, linkage

df = np.log(df)
aux = np.broadcast_to(df.values,  (df.shape[1], *df.shape))
#result = np.sqrt(np.square(aux - aux.transpose()).sum(axis=1))



#also possible:
res = scipy.spatial.distance.pdist(df.transpose(), metric='euclidean') #


###NEXT: Cluster based on this distance matrix!!!
# res is a condensed distance matrix

import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from sklearn.cluster import AgglomerativeClustering
import scipy.cluster.hierarchy as sch
clust=linkage(res, method="complete")


Z = sch.linkage(res, method='ward')
dendrogram = sch.dendrogram(Z)

from scipy.cluster.hierarchy import cut_tree
#Zijin: the first 0 relates to first column of df, which is the very earlierst date
print(cut_tree(Z, n_clusters = 2).T) # Printing transpose just for spa