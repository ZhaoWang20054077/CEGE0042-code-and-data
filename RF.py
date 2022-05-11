#!/usr/bin/env python
# coding: utf-8

# In[15]:


import pandas as pd 
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
import numpy as np 
from sklearn.metrics import (mean_squared_error, explained_variance_score, 
mean_absolute_error, r2_score) 
from sklearn.ensemble import RandomForestRegressor


# In[4]:


df = pd.read_csv('D:/STUDY/0042/factors.csv')


# In[5]:


df.columns = df.columns.str.strip()
df = df.drop('area code', axis=1)
df.shape


# In[6]:


y = df['cases'] # label
x = df.drop('cases', axis=1) # features
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.3, random_state=42)


# In[22]:


def model_result(clf,y_test,y_pred):
    MSE = mean_squared_error(y_test,y_pred)
    EVS = explained_variance_score(y_test, y_pred)
    MAE= mean_absolute_error(y_test, y_pred)
    r2score = r2_score(y_test, y_pred)
    print(f'MSE: {MSE:0.02f}')
    print(f'EVS: {EVS:0.02f}')
    print(f'MAE: {MAE:0.02f}')
    print(f'r2score: {r2score:0.02f}')


# In[23]:


rf_clf = RandomForestRegressor(n_estimators = 500,n_jobs = -1)
rf_clf.fit(x_train,y_train)


# In[24]:


y_pred_rf = rf_clf.predict(x_test)
model_result(rf_clf,y_test,y_pred_rf)


# In[26]:


y_pred_rf.shape


# In[20]:


importances = rf_clf.feature_importances_
std = np.std([tree.feature_importances_ for tree in rf_clf.estimators_],
             axis=0)
indices = np.argsort(importances)[::-1]
feature_list = [x.columns[indices[f]] for f in range(x.shape[1])]  #names of features.
ff = np.array(feature_list)

print("Feature ranking:")

for f in range(x.shape[1]):
    print("%d. feature %d (%f) name: %s" % (f + 1, indices[f], importances[indices[f]], ff[indices[f]]))


# In[21]:


plt.figure()
plt.rcParams['figure.figsize'] = [21, 10]
plt.title("Feature importances")
plt.bar(range(x.shape[1]), importances[indices],
       color="r", yerr=std[indices], align="center")
plt.xticks(range(x.shape[1]), ff[indices], rotation=90)
plt.xlim([-1, x.shape[1]])
plt.show()


# In[29]:


plt.figure()
plt.scatter(y_test,y_pred_rf)
plt.plot(y_test,y_test,color='red')
plt.xlabel("y_test")
plt.ylabel("y_pred")
plt.show()


# In[ ]:




