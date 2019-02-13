# k-means algorithm with 5 centers, run 20 times
kmeans(x, centers = 5, nstart = 20)

pokeman=read.csv("pokeman.txt")
pokeman

# Load the .csv files
apr14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-apr14.csv")

may14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-may14.csv")
jun14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jun14.csv")
jul14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv")
aug14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-aug14.csv")
sep14 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-sep14.csv")

#Let's bind all the data files into one. For this, you can use the bind_rows() function under the dplyr library in R.
library(dplyr)
data14 <- bind_rows(apr14, may14, jun14, jul14, aug14, sep14)

summary(data14)

#The dataset contains the following columns:
#Date.Time : the date and time of the Uber pickup;
#Lat: the latitude of the Uber pickup;
#Lon: the longitude of the Uber pickup;
#Base: the TLC base company code affiliated with the Uber pickup.

#Data Preparation
#This step consists of cleaning and rearranging your data so that you can work on it more easily.
#It's a good idea to first think of the sparsity of the dataset and check the amount of missing data.
# VIM library for using 'aggr'

install.packages("VIM")
library(VIM)
# 'aggr' plots the amount of missing/imputed values in each column
aggr(data14)

#Lubridate makes it simple for you to identify the order in which the year, month, and day appears in your dates and manipulate them.
library(lubridate)
# Separate or mutate the Date/Time columns
data14$Date.Time <- mdy_hms(data14$Date.Time)
data14$Year <- factor(year(data14$Date.Time))
data14$Month <- factor(month(data14$Date.Time))
data14$Day <- factor(day(data14$Date.Time))
data14$Weekday <- factor(wday(data14$Date.Time))
data14$Hour <- factor(hour(data14$Date.Time))
data14$Minute <- factor(minute(data14$Date.Time))
data14$Second <- factor(second(data14$Date.Time))

#data14$date_time
data14$Month

#Let's check out the first few rows to see what our data looks like now....
head(data14, n=10)

#K-means clustering is the most commonly used unsupervised machine 
#learning algorithm for dividing a given dataset into k clusters. 
#Here, k represents the number of clusters and must be provided by 
#the user.You already know k in case of the Uber dataset, which is 5
#or the number of boroughs. k-means is a good algorithm choice for
#the Uber 2014 dataset since you do not know the target labels making
#the problem unsupervised and there is a pre-specified k value. 
set.seed(20)
clusters <- kmeans(data14[,2:3], 5)
# Save the cluster number in the dataset as column 'Borough'
data14$Borough <- as.factor(clusters$cluster)

# Inspect 'clusters'
str(clusters)

#The above list is an output of the kmeans() function. Let's see some of the important ones closely:
#cluster: a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: a matrix of cluster centers.
#withinss: vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: total within-cluster sum of squares. That is, sum(withinss).
#size: the number of points in each cluster.

#Let's plot some graphs to visualize the data as well as the results of the k-means clustering well.
install.packages("ggmap")
library(ggmap)
NYCMap <- get_map("Nairobi", zoom = 10)

ggmap(NYCMap) + geom_point(aes(x = Lon[], y = Lat[], colour = as.factor(Borough)),data = data14) +
  ggtitle("NYC Boroughs using KMean")

#The boroughs (clusters) formed is matched against the real boroughs. The cluster number corresponds to the following boroughs:
#1.Bronx, 2.Manhattan, 3.Brooklyn, 4.Staten Island, 5.Queens

#You can use the borough information to check out Uber's growth within the boroughs for each month. Here's how... 
install.packages("DT")
library(DT)

data14$Month <- as.double(data14$Month)
month_borough_14 <- count_(data14, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_14)

#Let's get a graphical view of the same...

library(dplyr)
monthly_growth <- month_borough_14 %>%
mutate(Date = paste("04", Month)) %>%
ggplot(aes(Month, n, colour = Borough)) + geom_line() +
ggtitle("Uber Monthly Growth - 2014")
monthly_growth

#The biggest disadvantage is that it requires us to pre-specify the number 
#of clusters (k). However, for the Uber dataset, you had some domain knowledge
#available that told you the number of boroughs in New York City. This might
#not always be the case with real world datasets. Hierarchical clustering is
#an alternative approach that does not require a particular choice of clusters.




















