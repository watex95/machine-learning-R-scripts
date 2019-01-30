#change directory to where dataset is
setwd("C:/Users/hnyawate/Desktop/R_REPO")

#Load data
egypt_rice=read.csv("egypt_rice.csv",header=T)
dim(egypt_rice)

#convert to time series
RiceEgypt=ts(egypt_rice,frequency = 12,start = 2010,end=2018)
#just check to confirm its a time series
str(RiceEgypt)

#Package installation
#install the tseries package to perform stationarity test
library(tseries)
plot(RiceEgypt)
adf.test(RiceEgypt)


#FINANCIAL TIM(E SERIES ANALYSIS TO ANALYSE THE TREND OF RETURNS 
#RETURNS(Rt)=P(t)/P(t-1)
#log(Rt)=log(Pt)-log(Pt-1)

#DECOMPOSING THE SERIES
#get the log returns with lag=16 years back to remove the multiplicative seasonality
log_returns <- diff(log(RiceEgypt), lag=10)

#Test for stationarity of the returns
adf.test(log_returns)

#Difference the seiries to get a stationary series and test again for stationarity
log_returns1 = diff(log_returns, differences=1)
adf.test(log_returns1)


#PLOT ACF and PACF to get the order of the ARIMA model
acf(log_returns1) # number of lags that cut off the dotted line
#this indicate q value for the moving average model order(MA)

pacf(log_returns1)#number of lags that cut off the dotted line 
#this indicate value for p:the Autoregressive model order(AR) 

plot(log_returns1)#notice the plot is stationary coz spikes height is constant


#ARIMA ON THE LOG RETURNS
#Log returns are additive. Just add the daily returns together. 
#If you only have one average daily return you annualize simply by 
#multiplying with an annualization factor. Often 252 is used but it
#depends on your specific use case. In your case you may want to
#multiply by (days per year / days in 10 months period) Make sure 
#you do not apply the same to volatility. Volatility scales with the
#quare root of time
mymodel=arima(log_returns1,order = c(4,1,5))
mymodel

rice2019=predict(mymodel,n.ahead=12)
rice2019

















