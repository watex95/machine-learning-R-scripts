#Multivariate Adaptive Regression Splines
#Multivariate Adaptive Regression Splines (MARS) is a non-parametric regression 
#method that models multiple nonlinearities in data using hinge functions
#(functions with a kink in them).
# load the package
library(earth)
# load data
data(longley)
# fit model
fit <- earth(Employed~., longley)
# summarize the fit
summary(fit)
# summarize the importance of input variables
evimp(fit)
# make predictions
predictions <- predict(fit, longley)
# summarize accuracy
mse <- mean((longley$Employed - predictions)^2)
print(mse)

