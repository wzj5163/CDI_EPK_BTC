########################## BS
import matplotlib.pyplot as plt
import scipy.stats
import numpy as np
import pandas as pd
import os
path="/Users/ratmir/Downloads"
os.chdir(path)

#bootstrap epks:
df = pd.read_csv('epkuncmon.csv')
#estimated epks:
epks = pd.read_csv('epkallmon.csv')

#check:
df.plot(x="Return", legend=None)

epks.plot(x="Return")


df_epk = df.iloc[:, 1:df.shape[1]]
np_epk = df_epk.to_numpy()



#construct confidence intervals using z and sd:
sd_vec = np.std(df_epk, axis=1)
z = scipy.stats.norm.ppf(0.975)
ci_l1 = epks.iloc[:,1] - z * sd_vec
ci_u1 = epks.iloc[:,1] + z * sd_vec

#returns:
r = df.iloc[:,0]
#Using EPK estimated on whole sample:
plt.figure()
plt.scatter(r, ci_l1)
plt.scatter(r, ci_u1)
plt.plot(r, epks.iloc[:,1])

res = pd.concat( [r, epks.iloc[:,1], ci_l1, ci_u1], axis=1)

#interval = z * sd_vec
#res = pd.concat( [r, interval.rename("Interval")], axis=1)

res.to_csv("CI_monhtly_uncon.csv", sep=',')










#Using cluster 0:
df = pd.read_csv('epkc0mon.csv')
#check:
#df.plot(x="Return", legend=None)
df_epk = df.iloc[:, 1:df.shape[1]]
#np_epk = df_epk.to_numpy()

#construct confidence intervals using z and sd:
sd_vec = np.std(df_epk, axis=1)
z = scipy.stats.norm.ppf(0.975)
r = df.iloc[:,0]


ci_l2 = epks.iloc[:,2] - z * sd_vec
ci_u2 = epks.iloc[:,2] + z * sd_vec

plt.figure()
plt.scatter(r, ci_l2)
plt.scatter(r, ci_u2)
plt.plot(r, epks.iloc[:,2])


res = pd.concat( [r, epks.iloc[:,2], ci_l2, ci_u2], axis=1)
res.to_csv("monthly_c0.csv", sep=',')



#Using cluster 1:
df = pd.read_csv('epkc1mon.csv')
#check:
#df.plot(x="Return", legend=None)
df_epk = df.iloc[:, 1:df.shape[1]]
#np_epk = df_epk.to_numpy()

#construct confidence intervals using z and sd:
sd_vec = np.std(df_epk, axis=1)
z = scipy.stats.norm.ppf(0.975)
r = df.iloc[:,0]


ci_l3 = epks.iloc[:,3] - z * sd_vec
ci_u3 = epks.iloc[:,3] + z * sd_vec

plt.figure()
plt.scatter(r, ci_l3)
plt.scatter(r, ci_u3)
plt.plot(r, epks.iloc[:,3])

res = pd.concat( [r, epks.iloc[:,3], ci_l3, ci_u3], axis=1)
res.to_csv("monthly_c1.csv", sep=',')



#Using cluster 2:
df = pd.read_csv('Bootstrapped_EPK_CDI_4_4_c2.csv')
#check:
#df.plot(x="Return", legend=None)
df_epk = df.iloc[:, 1:df.shape[1]]
#np_epk = df_epk.to_numpy()

#construct confidence intervals using z and sd:
sd_vec = np.std(df_epk, axis=1)
z = scipy.stats.norm.ppf(0.975)
r = df.iloc[:,0]


ci_l4 = epks.iloc[:,4] - z * sd_vec
ci_u4 = epks.iloc[:,4] + z * sd_vec

plt.figure()
plt.scatter(r, ci_l4)
plt.scatter(r, ci_u4)
plt.plot(r, epks.iloc[:,4])

res = pd.concat( [r, epks.iloc[:,4], ci_l4, ci_u4], axis=1)
res.to_csv("CIc2.csv", sep=',')



