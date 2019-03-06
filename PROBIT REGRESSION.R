#A researcher is interested in how variables, such as GRE (Graduate Record 
#Exam scores), GPA (grade point average) and prestige of the undergraduate 
#institution, effect admission into graduate school. The response variable,
#admit/don't admit, is a binary variable.
install.packages("aod")
require(aod)
require(ggplot2)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

## convert rank to a factor (categorical variable)
mydata$rank <- factor(mydata$rank)

## view first few rows
head(mydata)

#describe the data 
summary(mydata)

#FIT THE MODEL
myprobit <- glm(admit ~ gre + gpa + rank, family = 
            binomial(link = "probit"),data = mydata)

## model summary
summary(myprobit)

#Confidence intervals
confint(myprobit)



#We can test for an overall effect of rank using the wald.test 
#function of the aod library. The order in which the coefficients
#are given in the table of coefficients is the same as the order
#of the terms in the model. This is important because the wald.test
#function refers to the coefficients by their order in the model.
#We use the wald.test function. b supplies the coefficients, 
#while Sigma supplies the variance covariance matrix of the error
#terms, finally Terms tells R which terms in the model are to be 
#tested, in this case, terms 4, 5, and 6, are the three terms 
#for the levels of rank.

wald.test(b = coef(myprobit), Sigma = vcov(myprobit), Terms = 4:6)
l <- cbind(0, 0, 0, 1, -1, 0)

wald.test(b = coef(myprobit), Sigma = vcov(myprobit), L = l)

#We can also use predicted probabilities to help you understand the model.
#To do this, we first create a data frame containing the values we want for
#the independent variables.

newdata<-data.frame(gre=rep(seq(from=200,to=800,length.out=100),4*4),
gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4),
rank = factor(rep(rep(1:4,each = 100), 4)))

newdata[,c("p","se")]<-predict(myprobit,newdata,
            type="response",se.fit = TRUE)[-3]

ggplot(newdata, aes(x = gre, y = p, colour = rank)) +
  geom_line() + facet_wrap(~gpa)
head(newdata)


## change in deviance
with(myprobit, null.deviance - deviance)

## change in degrees of freedom
with(myprobit, df.null - df.residual)

## chi square test p-value
with(myprobit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

logLik(myprobit)








