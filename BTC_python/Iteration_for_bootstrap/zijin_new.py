import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os
path="/Users/ratmir/Downloads"

os.chdir(path)


# IMPORT WEEKLY Q DENSITIES
df = pd.read_csv('Qweek.csv')


N = 194 #194
df = df.iloc[ :, 1:N]
cols = list(df.columns)
cols.reverse()
df = df[cols] #reverse cols
draws = 400 # mach 400

#df.sample(n=draws, axis='columns', replace=True)

B=100

arr_fill = np.zeros((B, df.shape[0] , draws))

for b in range(B):
arr_fill[b,:,:] = df.sample(n=draws, axis='columns', replace=True) # (B, GRID, SUBSAMPLES)

result=np.reshape(arr_fill, (B*df.shape[0], draws)) #
result=np.around(result, decimals=4)
np.savetxt("BSfull.csv", result, delimiter=",")

###########FOR 3 CLUSTERS:

cluster = np.array([0, 0, 0, 1, 2, 1, 0, 1, 1, 0, 0, 2, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 2, 0, 0,
  1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2,
  2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
  1, 1, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 2, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 2, 2, 2, 2,
  1, 1, 1, 1, 2, 1, 2, 2, 2, 0, 1, 2, 2])

# Split the DataFrame based on the cluster code
clusters = np.unique(cluster)
dfs = []
for c in clusters:
    dfs.append(df.iloc[:, cluster == c])


df0 =dfs[0]
df1 =dfs[1]
df2 =dfs[2]

arr_fill0 = np.zeros((B, df0.shape[0] , draws))
arr_fill1 = np.zeros((B, df1.shape[0] , draws))
arr_fill2 = np.zeros((B, df2.shape[0] , draws))


draws = 200;
for b in range(B):
    arr_fill0[b,:,:] = df0.sample(n=draws, axis='columns', replace=True) # (100, 1002, 200) wird es sein
    arr_fill1[b,:,:] = df1.sample(n=draws, axis='columns', replace=True) # (100, 1002, 200) wird es sein
    arr_fill2[b,:,:] = df2.sample(n=draws, axis='columns', replace=True) # (100, 1002, 200) wird es sein

result0=np.reshape(arr_fill0, (B*df0.shape[0], draws)) # wird dimension (100200, 200) haben.
result0=np.around(result0, decimals=4)
np.savetxt("BSc0.csv", result0, delimiter=",")

result1=np.reshape(arr_fill1, (B*df1.shape[0], draws)) # wird dimension (100200, 200) haben.
result1=np.around(result1, decimals=4)
np.savetxt("BSc1.csv", result1, delimiter=",")

result2=np.reshape(arr_fill2, (B*df2.shape[0], draws)) # wird dimension (100200, 200) haben.
result2=np.around(result2, decimals=4)
np.savetxt("BSc2.csv", result2, delimiter=",")










