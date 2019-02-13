#BAYESIAN DECISION THEORY

#What price should I sell my used phone for?
#Imagine I have an unscratched used phone that was made in 2014
#that I want to sell. I might go online and look at some closed
#eBay listings and see what sold and what didn't. Lets pretend I
#believe that there are 3 key variables that dictate whether a 
#phone sold or not: whether it was scratched or not, the year it 
#was made, and the price it was listed at. Given this information
#I want to figure out what price I should list my phone at. In
#particular I want to maximize my expected return (the expected
#amount of money I will make), this defines my loss function as
#I will discuss below.

library(tidyverse)
library(brms)

d <- data.frame(sold = c(1, 1, 0, 0, 1, 1), 
                scratched = c(0, 0, 1, 0, 0, 0), 
                year = c(2014, 2015, 2010, 2014, 2015, 2016), 
                price = c(50, 70, 40, 100, 90, 100))
d

#We will use a default uniform prior on ??0 and we will place informative
#priors on the other coefficients. For example, since I believe
#that having a scratch would likely decrease the probability that
#a phone sold I will use the following prior ??1???Normal(???1,1).

fit <- brm(sold ~ scratched + year + price, data = d, family = bernoulli(link = "logit"), 
           prior = c(set_prior("normal(-1,1)", class="b", coef="scratched"), 
                     set_prior("normal(1, 1)", class="b", coef="year"), 
                     set_prior("normal(-2, 1)", class="b", coef="price")), 
           silent=TRUE, 
           refresh = -1)

summary(fit)


loss <- function(price){
  # First Create the input data to predict with out model - we want to predict whether or not our phone will sell
  our.phone <- data.frame(scratched=0, year=2014, price=price) 
  
  # Next, for each posterior sample from out model, predict whether or not our phone would sell at the given price. This will give a vector of 0's and 1's, did the phone sell in each posterior sample. Think of each posterior sample as a simulation. 
  pp <- posterior_predict(fit, newdata=our.phone)
  
  # Next calculate the expected return for each of these posterior simulations
  mean(pp*price)
}


#Now that we have a function implementing the calculation of expected loss for each 
#price we can optimize this to find the listing price that maximizes
#our expected return. Again because our "loss" function is something
#that in this case we want to maximize I will instead pass it to R's 
#optim function as a negated version of itself (add a negative sign) 
#as the optim function defaults to minimizing a passed function.

(op <- optim(50, function(x) -loss(x)))


#Finally lets look at what this expected return/loss looks 
#like evaluated at a number of different listing prices.

x <- 10:110 # Listing prices to evaluate
l <- sapply(x, loss) 
plot(x, l, xlab = "Listing Price", ylab = "Expected Return")

#Basically if we list our phone for a low-enough value (e.g., for 10) 
#we are almost certain that the phone will sell but we won't
#make much money for it and on average we can expect to make 
#about 10. At the other extreme if we list our phone for too 
#much (e.g., for 110) then we are unlikely to sell it but if 
#we do sell it, it will make much more money. Where is the 
#best balance in terms of maximizing the expected return? 
#Right around 71 where we can expect to make about 58 on
#average, just as we saw from our black-box optimization 
#using the optim function.

















