# Data loading and cleaning steps:

#directory path
#path <- ".../Data/Big_Mart_Sales"

#set working directory
setwd("C:/Users/hnyawate/Desktop")

pca_na=read.csv("PCA_NULL.csv",header = T)


#impute missing values with median
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)



pca_na$PctHousNoPhone[is.na(pca_na$PctHousNoPhone)]<-median(pca_data$PctHousNoPhone,na.rm = TRUE)
is.na(pca_data$PctHousNoPhone)


#load train and test file
set.seed(123)#parameter for randomness
pca_sample=sample(2215,2000)#random sampling
pca_train=pca_data[pca_sample,]#splitting for x_train
pca_test=pca_data[-pca_sample,]#s


#add a column
test$Item_Outlet_Sales <- 1

#combine the data set
combi <- rbind(train, test)






#impute 0 with median
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),

#find mode and impute
table(combi$Outlet_Size, combi$Outlet_Type)
levels(combi$Outlet_Size)[1] <- "Other"                                 

#we are practicing an unsupervised learning technique, hence 
#response variable must be removed.
#remove the dependent and identifier variables
my_data<-subset(combi,select= -c(Item_Outlet_Sales, Item_Identifier,                               
                                  
#Let's check the available variables ( a.k.a predictors) in the data set.
#check available variables
colnames(my_data)                             


#Since PCA works on numeric variables, let's see if we have any
#variable other than numeric.
#check variable class
str(my_data)

#Sadly, 6 out of 9 variables are categorical in nature. We have
#some additional work to do now. We'll convert these categorical
#variables into numeric using one hot encoding.
#load library
library(dummies)

#create a dummy data frame
new_my_data <-dummy.data.frame(my_data,names = c("Item_Fat_Content",
"Item_Type","Outlet_Establishment_Year","Outlet_Size",
"Outlet_Location_Type","Outlet_Type"))
#To check, if we now have a data set of integer values, simple write:
#check the data set
str(new_my_data)

#And, we now have all the numerical values. Let's divide the data
#into test and train.
#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.test <- new_my_data[-(1:nrow(train)),]

#We can now go ahead with PCA.

#The base R function prcomp() is used to perform PCA. By default, 
#it centers the variable to have mean equals to zero. With 
#parameter scale. = T, we normalize the variables to have 
#standard deviation equals to 1.
#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

#The prcomp() function results in 5 useful measures:
#1. center and scale refers to respective mean and standard
#deviation of the variables that are used for normalization 
#prior to implementing PCA
#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#2. The rotation measure provides the principal component 
#loading. Each column of rotation matrix contains the principal
#component loading vector. This is the most important measure
#we should be interested in.

prin_comp$rotation

#This returns 44 principal components loadings. Is that correct ? 
#Absolutely. In a data set, the maximum number of principal 
#component loadings is a minimum of (n-1, p). Let's look at 
#first 4 principal components and first 5 rows.

prin_comp$rotation[1:5,1:4]

#3. In order to compute the principal component score vector,
#we don't need to multiply the loading with data. Rather, the
#matrix x has the principal component score vectors in a 
#8523 × 44 dimension.
dim(prin_comp$x)


#Let's plot the resultant principal components.
biplot(prin_comp, scale = 0)

#The parameter scale = 0 ensures that arrows are scaled to 
#represent the loadings. To make inference from image above, 
#focus on the extreme ends (top, bottom, left, right) of this graph.

#4. The prcomp() function also provides the facility to compute
#standard deviation of each principal component. sdev refers to 
#the standard deviation of principal components.

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]


#We aim to find the components which explain the maximum variance.
#This is because, we want to retain as much information as possible
#using these components. So, higher is the explained variance,
#higher will be the information contained in those components.

#To compute the proportion of variance explained by each component, 
#we simply divide the variance by sum of total variance. This results in:
  
 #proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#This shows that first principal component explains 10.3% variance.
#Second component explains 7.3% variance. Third component explains 6.2%
#variance and so on. So, how do we decide how many components
#should we select for modeling stage ?

#The answer to this question is provided by a scree plot. A scree 
#plot is used to access components or factors which explains
#the most of variability in the data. It represents values in
#descending order.

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
                                  
                                  
#The plot above shows that ~ 30 components explains around 98.4% 
#variance in the data set. In order words, using PCA we have
#reduced 44 predictors to 30 without compromising on explained
#variance. This is the power of PCA> Let's do a confirmation
#check, by plotting a cumulative variance plot. This will give
#us a clear picture of number of components.

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")   

#This plot shows that 30 components results in variance close to ~ 98%. Therefore,
#in this case, we'll select number of components as 30 [PC1 to PC30] 
#and proceed to the modeling stage. This completes the steps to
#implement PCA on train data. For modeling, we'll use these 30
#components as predictor variables and follow the normal procedures.


#Predictive Modeling with PCA Components

#add a training set with principal components
train.data <- data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:31]

#run a decision tree
install.packages("rpart")
library(rpart)
rpart.model <- rpart(Item_Outlet_Sales ~ .,data = train.data, method = "anova")
rpart.model

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:30]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)

#For fun, finally check your score of leaderboard
sample <- read.csv("SampleSubmission_TmnO39y.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
write.csv(final.sub, "pca.csv",row.names = F)

#That's the complete modeling process after PCA extraction.
#I'm sure you wouldn't be happy with your leaderboard rank 
#after you upload the solution. Try using random forest!
























                             