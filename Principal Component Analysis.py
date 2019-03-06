

# For Python Users: To implement PCA in python, simply import PCA from sklearn library. The interpretation remains same as explained for R users above. Ofcourse, the result is some as derived after using R. The data set used for Python is a cleaned version where missing values have been imputed, and categorical variables are converted into numeric. The modeling process remains same, as explained for R users above.

import numpy as np
from sklearn.decomposition import PCA
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import scale
%matplotlib inline

#Load data set
data = pd.read_csv('Big_Mart_PCA.csv')

#convert it to numpy arrays
X=data.values

#Scaling the values
X = scale(X)

pca = PCA(n_components=44)

pca.fit(X)

#The amount of variance that each PC explains
var= pca.explained_variance_ratio_

#Cumulative Variance explains
var1=np.cumsum(np.round(pca.explained_variance_ratio_, decimals=4)*100)

print var1

plt.plot(var1)

#Looking at above plot I'm taking 30 variables
pca = PCA(n_components=30)
pca.fit(X)
X1=pca.fit_transform(X)

print X1

# Points to Remember

#     PCA is used to overcome features redundancy in a data set.
#     These features are low dimensional in nature.
#     These features a.k.a components are a resultant of normalized linear combination of original predictor variables.
#     These components aim to capture as much information as possible with high explained variance.
#     The first component has the highest variance followed by second, third and so on.
#     The components must be uncorrelated (remember orthogonal direction ? ). See above.
#     Normalizing data becomes extremely important when the predictors are measured in different units.
#     PCA works best on data set having 3 or higher dimensions. Because, with higher dimensions, it becomes increasingly difficult to make interpretations from the resultant cloud of data.
#     PCA is applied on a data set with numeric variables.
#     PCA is a tool which helps to produce better visualizations of high dimensional data.

 










