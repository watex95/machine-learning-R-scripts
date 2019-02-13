
install.packages("tidyverse")
#Fitting Non-Linear Growth Curves in R 
library(tidyverse)
set.seed(4)

#Note: this function assumes that the data/time vectors are 
#ordered with respect to time (this is because of the way I 
#use the diff function to pick starting values. )

fit.gompertz <- function(data, time){
  d <- data.frame(y=data, t=time)
  
  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  starting.values <- c(a=max(d$y), 
                       mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=i)
  print("Starting Values for Optimization: ")
  print(starting.values)
  ##########################
  
  formula.gompertz <- "y~a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))"
  nls(formula.gompertz, d, starting.values)
}


#Now we are going to create some simulated data to test this on.

gompertz <- function(time, a, mu, lambda){
  y <- a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))
  return(data.frame(time=time, y=y))
}

d <- gompertz(1:100, 10, 2, 30)
plot(d)



#This is just the deterministic gompertz function. Now lets 
#add some measurement noise
# Add some normal(0,0.5) noise centered around the deterministic signal
for(i in 1:nrow(d)) d[i,2] <- rnorm(1, d[i,2], 1)

#Now fit the noisy data and and plot the resulting fitted model.
(fit <- fit.gompertz(d$y, d$time))

plot(d, ylab="microbial abundance")
lines(d$time, predict(fit))

#One thing to note, if you find that the fit.gompertz function
#gives an error warning about a singular gradient: the problem 
#is almost certanly that the starting values are far from the 
#optimal and you should plot the data and estimate better values.
#I also would suggest using the purrr::safely function to fit 
#many curves at once. This is because the nls function often 
#returns errors for poorly fit models and its a pain to have 
#to keep excluding data-points/curves manually (easier to just 
#collect the errors).

#Below I create a "safe" version of the fit.gompertz function that collects errors rather than stopping evaluation.
safe.fit.gompertz <- safely(fit.gompertz)


#To demonstrate how this works lets try it out on a growth curve with only 2 datapoints (something we know) will throw an error.
safe.fit.gompertz(c(1,2), c(19, 19))

#A low AIC is suggestive of poor model fit. In practice I found it useful to fit each growth curve and report the AIC value;
#then I would sort the fitted models by AIC and visually inspect the fitted models with the lowest AIC. This allowed me to 
#quickly choose which growth curves were likely problematic so my friend could go back and collect those measurements again. 

AIC(fit)



#Non linear models ,logistic models 
#R codes


data(Uspop)
#generating the theta values,theta1 is default value
lm(logit(y/theta1)~x,Uspop)

#the main model
model<-nls(y~theta1/(1+exp(-(theta2+theta3*x))),start=list(theta1=400,theta2=-49,theta3=0.25),data=Uspop,trace=TRUE)
#prints model report 
summary (model)

#std error of an estimate
deltaMethod(model,"theta3")

#Visualisation and prediction
plot(y~x,Uspop,xlim=c(1790,2100),ylim=c(0,450))
with(Uspop,lines(seq(1790,2100,by=10),
                 predict(model, data.frame(x=seq(1790,2100,by=10))),led=2))
points (2010,307,pch="x",cex=1.3)
abline(h=0,lty=2)
abline(h,coef(model)[1],lty=2)
abline(h=0.5*coef(model)[1],lty=2)
abline(v=-coef(model)[1]/coef(model)[3],lty=2)

#Residual plots 
with(Uspop,plot(y,residuals(model),type='b'))








