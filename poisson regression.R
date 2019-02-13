# installing/loading the latest installr package:
install.packages("installr"); library(installr) # install+load installr

updateR() # updating R.


#-----------------------------------------------------------------------------

getwd()
install.packages("ggplot2")
install.packages("msm")
p<-read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")

attach(p)
prog<-factor(p$prog,levels=1:3,labels=c("General","Academic","Vocational"))
table(prog)

id <- factor(id)
summary(p)

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")


summary(m1<-glm(num_awards ~ prog + math, family="pisson",data=p))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#The output begins with echoing the function call. The information on
#deviance residuals is displayed next. Deviance residuals are approximately
#normally distributed if the model is specified correctly.In our example,
#it shows a little bit of skeweness since median is not quite zero.
#Next come the Poisson regression coefficients for each of the variables
#along with the standard errors, z-scores, p-values and 95% confidence 
#intervals for the coefficients. The coefficient for math is .07. This 
#means that the expected log count for a one-unit increase in math is .07.
#The indicator variable progAcademic compares between prog = "Academic" 
#and prog = "General", the expected log count for prog = "Academic" 
#increases by about 1.1. The indicator variable prog.Vocational is the
#expected difference in log count ((approx .37)) between prog = "Vocational"
#and the reference group (prog = "General").
#The information on deviance is also provided. We can use the residual
#deviance to perform a goodness of fit test for the overall model. The 
#residual deviance is the difference between the deviance of the current
#model and the maximum deviance of the ideal model where the predicted
#values are identical to the observed. Therefore, if the residual difference
#is small enough, the goodness of fit test will not be significant, 
#indicating that the model fits the data. We conclude that the model fits
#reasonably well because the goodness-of-fit chi-squared test is not
#statistically significant. If the test had been statistically significant,
#it would indicate that the data do not fit the model well. In that situation,
#we may try to determine if there are omitted predictor variables, if our
#linearity assumption holds and/or if there is an issue of over-dispersion.


#We can also test the overall effect of prog by comparing the deviance
#of the full model with the deviance of the model excluding prog.The two
#degree-of-freedom chi-square test indicates that prog, taken together,
#is a statistically significant predictor of num_awards.

## update m1 model dropping prog
m2 <- update(m1, . ~ . - prog)
## test model differences with chi square test
anova(m2, m1, test="Chisq")


#Sometimes, we might want to present the regression results as incident
#rate ratios and their standard errors, together with the confidence
#interval. To compute the standard error for the incident rate ratios,
#we will use the Delta method. To this end, we make use the function 
#deltamethod implemented in R package msm.
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m1), cov.m1)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s
rexp.est

#The output above indicates that the incident rate for prog = "Academic" 
#is 2.96 times the incident rate for the reference group (prog = "General").
#Likewise, the incident rate for prog = "Vocational" is 1.45 times the
#incident rate for the reference group holding the other variables at 
#constant. The percent change in the incident rate of num_awards is by 7% 
#for every unit increase in math
(s1 <- data.frame(math = mean(p$math),
prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))

predict(m1, s1, type="response", se.fit=TRUE)
#In the output above, we see that the predicted number of events for 
#level 1 of prog is about .21, holding math at its mean. The predicted
#number of events for level 2 of prog is higher at .62, and the predicted
#number of events for level 3 of prog is about .31. The ratios of these
#predicted counts ((frac{.625}{.211} = 2.96), (frac{.306}{.211} = 1.45)) 


# calculate and store predicted values
p$phat <- predict(m1, type="response")

## order by program and then by math
p <- p[with(p, order(prog, math)), ]

## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")

#The graph indicates that the most awards are predicted for those in the 
#academic program (prog = 2), especially if the student has a high math 
#score. The lowest number of predicted awards is for those students in
#the general program (prog = 1).

#THINGS TO CONSIDER
#Assuming that the model is correctly specified, the assumption that the
#conditional variance is equal to the conditional mean should be checked.
#There are several tests including the likelihood ratio test of over-dispersion
#parameter alpha by running the same model using negative binomial distribution.
#R package pscl (Political Science Computational Laboratory, Stanford University)
#provides many functions for binomial and count data including odTest for testing
#over-dispersion.


