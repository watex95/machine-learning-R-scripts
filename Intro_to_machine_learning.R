
?USArrests
head(USArrests)
dim(USArrests)
str(USArrests)
summary(USArrests)

cor(y=USArrests$Murder,x=USArrests)


install.packages('corrgram')
library(corrgram)

corrgram(USArrests,lower.panel=panel.shade,upper.pane=panel.shade)
install.packages('ggplot2')

library(ggplot2)

library(cowplot)
g0=qplot(x-Murder,y=Murder,data=USArrests)
g1=qplot(x=Assault,y=Murder,data=USArrests)
g2=qplot(x=Urbanpop,y=Murder,data=USA)
g3=qplot(x=Rape,y=Murder,data=USArrests)

plot_grid(g0,g1,g2,g3,labels='Murder','Urbanpop','Rape',ncol=2,nrow=2)


#CREATING TEST AND TRAIN DATASET

set.seed(1)
sample(20:60,5,replace=TRUE)
set.seed(1)
m=nrow(USArrests)
train_index=sample.int(m,size=m*0.70)
train=USArrests[train_index,]
test=USArrests[-train_index,]
dim(train)
dim(test)
str(test)
edit(USArrests)
attach(USArrests)
#x and y split
x_train=train[,c('Assault','UrbanPop','Rape')]
x_test=test[,c('Assault','UrbanPop','Rape')]
x_test
y_train=train$Murder
y_test=test$Murder

#train model
model=lm(Murder~.,data=USArrests)
model
summary(model)
new_record=list(Assault=12,Rape=20,UrbanPop=15)
predicted_murder_arrests=predict(model,new_record)
predicted_murder_arrests #THIS NUMBER IS EXPLAINED AS PER 100,000 OCCURRENCES.
USArrests
#SCORING THE MODEL
#predict test set
y_pred=predict(model,x_test)
y_pred
#add prediction and errors to the data.frame
test$murder_predicted=y_pred
test$error=test$murder_predicted-y_test

test[,c('Murder','murder_predicted','error')]

#R SQUARED
r_squared=function(pred,actual){
error=pred-actual
error_sq=error^2
regress=pred-mean(pred)
regress_sq=regress^2
return(1-sum(error_sq)/sum(regress_sq))
}

r_squared(y_pred,test$Murder)

#RMSE ROOT MEAN SQUARED ERROR
rmse=function(pred,actual){
error=pred-actual
sq_error=error^2
mean_sq_error=mean(sq_error)
return(sqrt(mean_sq_error))
}
rmse(y_pred,test$Murder)

##RESIDUAL PLOT
library(ggplot2)

model_residual=resid(model)
model_residual
test_residuals=y_pred-test$Murder
test_residuals
test$Assault

qplot(x=train$Assault,y=model_residual,xlab='Assault',ylab='Residual',color='Train Data')+
geom_point(aes(x=test$Assault,y=test_residuals,color='Test data'))+
geom_abline(intercept=c(0,0),slope=0)+
scale_y_continuous(limits=c(-6,6))

##PLOTTING THE RESULTS
#Plotting the actual vs predicted values
actual_vs_predicted=function(x=test$Murder,y=test$murder_predicted){
  qplot(x=x,y=y)
  xlab('Actual murder rate')
  ylab('predicted murder rate')
scale_x_continuous(breaks = seq(5,20,5))
scale_y_continuous(breaks = seq(5,20,5))
  }

actual_vs_predicted()

#Making an adjustment to to the previous values
#lests try a minor improvement
model2=lm(Murder~Assault+Rape,data = train)
model2
test$murder_predicted_model2=predict(model2,test)
test$murder_predicted_model2

#looks a little better
actual_vs_predicted(y=test$murder_predicted_model2)
#what do the metrics say
r_squared(test$murder_predicted,test$Murder)
r_squared(test$murder_predicted_model2,test$Murder)

rmse(test$murder_predicted,test$Murder)
rmse(test$murder_predicted_model2,test$Murder)

model1_residuals=test$murder_predicted-test$Murder
model2_residuals=test$murder_predicted_model2-test$Murder

qplot(x=train$Assault,y=model1_residuals,xlab='Assault',ylab='Residual',color='Model 1')+
  geom_point(aes(x=test$Assault,y=model2_residuals,color='Model 2'))+
  geom_abline(intercept=c(0,0),slope=0)+
  scale_y_continuous(limits=c(-6,6))


test[,c('Murder','murder_predicted','murder_predicted_model2')]

##CLASSIFICATION
#Determining car engine size based on other characteristics
library(ggplot2)
head(mtcars)
?mtcars

examine_lvar=function(feature,title=''){
  qplot(cyl,feature,color=factor(cl),main=title,data=mtcars,
        geom="points",xlab=xlab,ylab=ylab)
}

examine_2var=function(feature,title=''){
  qplot(feature1,feature2,color=factor(mtcars$c1),main=title,data=mtcars,
        geom="points",xlab=xlab,ylab=ylab)
}


examine_lvar(mtcars$disp,'displacement')
examine_2var(mtcars$disp,mtcars$hp,'displacement','horsepower')
examine_2var(mtcars$qsec,mtcars$wt,'1/4 mile','weight')


##WORKING WITH FACTORS
mtcars
summary(mtcars)
mtcars$cyl=factor(mtcars$cyl,labels=c('four cylinder','six cylinder','eight cylinder'))
mtcars$cyl
mtcars$am=factor(mtcars$am,labels = c('automatic','manual'))
mtcars$carb=factor(mtcars$carb)
mtcars$gear=factor(mtcars$gear)
mtcars$vs=factor(mtcars$vs,labels = c('V','straight'))
head(df)

summary(mtcars[c('cyl','am','carb','gear','vs')])

labels=model.matrix(~cyl-1,data = mtcars)
labels


##SCALING THE DATA
library(caret)
df=mtcars
summary(df)
df$cyl=factor(df$cyl,labels=c('four cylinder','six cylinder','eight cylinder'))
df$cyl
df$am=factor(df$am,labels = c('automatic','manual'))
df$carb=factor(df$carb)
df$gear=factor(df$gear)
df$vs=factor(df$vs,labels = c('V','straight'))

scaler=function(x){
  return(x-min(x)/max(x)-min(x))
}
df$mpg_s=scaler(df$mpg)
df$disp_s=scaler(df$disp)
df$hp_s=scaler(df$hp)
df$drat_s=scaler(df$drat)
summary(df)


##BUILD A CLASSIFICATION MODEL: Logistic regression multi-class classification model
m=nrow(mtcars)
set.seed(1)
train_idx=sample(m,m*.7)
train=df[train_idx,]
test=df[-train_idx,]
library(nnet)
model=multinom(cyl~., data = train) #neuralnet multinomial model
summary(model)

test$predicted=predict(model,test)

acc=sum(test$predicted==test$cyl)/nrow(test)
print(paste('accuracy on the test  set is',round(acc*100,2),"%",sep=))


#another way we can run the model is using interractions of the variables as follows
model_x=multinom(cyl~hp:wt+mpg:wt+carb:wt+disp:wt, data = train) #neuralnet multinomial model
model_x
summary(model_x)
test$predicted=predict(model_x,test)
acc1=sum(test$predicted==test$cyl)/nrow(test)
print(paste('accuracy on the test  set is',round(acc*100,2),"%",sep=))

##another way where the power raised indicates the magnitude of interraction ie 2 variables  as follows 
model_f=multinom(cyl~(hp+mpg+qsec+wt)^2, data = train) #neuralnet multinomial model
model_f
summary(model_f)
test$predicted=predict(model_f,test)
acc2=sum(test$predicted==test$cyl)/nrow(test)
print(paste('accuracy on the test  set is',round(acc*100,2),"%",sep=))

#to exclude some features ie carb use the following syntax
model3=multinom(cyl~.-carb,data=train)
model3 
test$predicted=predict(model3,test)
acc2=sum(test$predicted==test$cyl)/nrow(test)
print(paste('accuracy on the test  set is',round(acc*100,2),"%",sep=))

##Precision recall and F-score





#Introduction to caret package
#EDA and preprocessing 
install.packages('RANN')

library(RANN)

#Exploring dataset
install.packages('mlbench')
library(mlbench)
library(ggplot2)
data("PimaIndiansDiabetes2")
pid=PimaIndiansDiabetes2
head(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
?PimaIndiansDiabetes2

examine_1var=function(feature,title=''){
  qplot(diabetes,feature,color=diabetes,main=title,data =pid,geom='boxplot')
}
examine_1var=function(feature1,feature2,xlab,ylab){
  qplot(feature1,feature2,color=diabetes,data =pid,geom='point',xlab=xlab,ylab = ylab)
}
#call the functions with each variable to view the plot 
examine_1var(pid$glucose,'glucose')
examine_1var(pid$pregnant,'pregnant')
examine_1var(pid$pressure,'pressure')
examine_1var(pid$triceps,'triceps')
examine_1var(pid$insulin,'insulin')
examine_1var(pid$mass,'mass')
examine_1var(pid$pedigree,'pedigree')
examine_2var(pid$glucose,pid$mass,'glucose','mass')
examine_2var(pid$glucose,pid$insulin,'glucose','insulin')
examine_2var(pid$age,pid$mmass,'age','mass')
examine_2var(pid$pregnant,pid$mass,'pregnant','mass')
examine_2var(pid$pregnant,pid$age,'pregnant','age')

##Dealing with missing data
library(mlbench)
library(caret)
library(ggplot2)
pid=PimaIndiansDiabetes2
missing=function(col){
  return(sum(is.na(col)))
}

sapply(pid,missing)
#make a copy
imputed_glucose=pid$glucose
#simple fix
summary(imputed_glucose)
imputed_glucose[is.na(imputed_glucose)]<-mean(imputed_glucose,na.rm=TRUE)
summary(imputed_glucose)

#caret allows us to impute data and scale data ie preprocessing
?preProcess
pid_imputed=predict(impute_model,pid)
sapply(pid_imputed,missing)

#examine results
head(pid$glucose)
head(pid_imputed$glucose)
pid_imputed$glucose[is.na(pid$glucose)]

#Train and test splits stratified by default
train_indx<-createDataPartition(pid$diabetes,p=.7,list=FALSE)
train2=pid[train_indx,]
test2=pid[-train_idx,]
dim(train2)
dim(test2)

#verify split size
length(train_indx)/length(pid$diabetes)
dim(train2)[1]/length(pid$diabetes)

#creating the model
model_new<-train(diabetes~.,
                 method='glm',
                 data=pid,
                 na.action=na.pass,
                 preProcess=c('knnImpute')
                 )
summary(model_new)

test2$predicted=predict(model,newdata=test,na.action=na.pass)

#accuracy
sum(test2$predicted==test2$diabetes)/nrow(test2)

#Cross validation
train_control=trainControl(method='cv',
                           number=10,
                           savePredictions=TRUE)
model=train(diabetes~.,
            method='glm',
            data=pid,
            trControl=train_control,
            na.action=na.pass,
            preProcess=c('knnImpute')
)
summary(model)

#accuracy
sum(model$pred$pred==model$pred$obs)/nrow(model$pred)

##F-SCORE
#Confusion matrix
cm=confusionMatrix(model$pred$pred,model$pred$obs,positive='pos')
cm$overall
cm$table #confusion table
cm$byClass[['F1']] #F-score
cm$byClass #displays all the measures













