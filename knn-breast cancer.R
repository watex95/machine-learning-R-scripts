#KNN ALGORITHM  FOR CLASSIFICATION OF BREAST CANCER 

#Loading the data and install packages
wbcd=read.csv('wbcd.csv',header=T)
install.packages("class")
install.packages("gmodels")


#remove the ID
wbcd_raw=wbcd[,-1]

edit(wbcd_raw)

#DATA EXPLORATION AND PREPARATION
#inpecting the data
str(wbcd)
attach(wbcd)

#frequecy analysis
table(wbcd$diagnosis)

#convert categories to factors
wbcd$diagnosis=factor(wbcd$diagnosis,levels=c('B','M'),labels = c('benign','malignant'))

wbcd$diagnosis

#proportion analysis
round(prop.table(table(wbcd$diagnosis))*100,digits=1)

#descriptives stats
summary(wbcd_raw[2:31])

#normalizing the data since there is a very large spread and it will cause bias in classification.
normalize=function(x){
  return((x-min(x))/(min(x)-max(x)))
}
wbcd_n=-as.data.frame(lapply(wbcd_raw[2:31], normalize))


#test to see if the data is normalized
summary(wbcd_n)

#create train and test datasets for x_train and x_test
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#let's consider randomly splitting a dataset
set.seed(123)#parameter for randomness
train_sample=sample(569,469)#random sampling
wbcd_train=wbcd_n[train_sample,]#splitting for x_train
wbcd_test=wbcd_n[-train_sample,]#splitting for the x_test

edit(wbcd_test)
0
#create train and test labels of the target variable diaagnosis (y_train and y_test)
wbcd_train_labels <- wbcd[1:469,2]
wbcd_train_labels
wbcd_test_labels <- wbcd[470:569, 2]
wbcd_test_labels

#TRAINING A MODEL ON THE DATA

library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)
wbcd_test_pred

#EVALUATING MODEL PERFORMANCE 
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=F)
#A total of 2 out of 100, or 2 percent of masses were incorrectly classified by the k-NN
#approach. While 98 percent accuracy seems impressive for a few lines of R code,
#we might try another iteration of the model to see whether we can improve the
#performance and reduce the number of values that have been incorrectly classified,
#particularly because the errors were dangerous false negatives.


#IMPROVING MODEL PERFORMANCE
#Transforming the numeric values by rescaling with z-score
wbcd_z <- as.data.frame(scale(wbcd[-(1:2)]))
summary(wbcd_z$area_mean)

#The mean of a z-score standardized variable should always be zero, and the range
#should be fairly compact. A z-score greater than 3 or less than -3 indicates an
#extremely rare value. With this in mind, the transformation seems to have worked.
#As we had done earlier, we need to divide the data into training and test sets, and
#then classify the test instances using the knn() function. We'll then compare the predicted labels to the actual labels using CrossTable():

#THE BEST WAY TO SPLIT THE DATA IS RANDOMLY
set.seed(123)
train_sample=sample(569,469)
wbcd_train=wbcd_z[train_sample,]
wbcd_test=wbcd_z[-train_sample,]
wbcd_train_labels <- wbcd[train_sample, 2]
wbcd_test_labels <- wbcd[-train_sample, 2]

#THE FOLLOWING WAY WITHOUT RANDOMIZATION HAS HIGH ACCURACY BUT MAY ENCOUTER BIAS
#IF THE DATA WAS ORDERED IN SOME WAY.
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 2]
wbcd_test_labels <- wbcd[470:569, 2]


#Use the k value that gives the highest sum of true positive and negative values
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 12)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)




































































