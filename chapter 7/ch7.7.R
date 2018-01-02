#----------------------ch7---(7)--------------------------------------
library(ISLR)
library(gam)
library(boot)
library(MASS)
dim(Wage) #3000x12
names(Wage)#year,age,sex,maritl,race,education,region,jobclass,health,health_ins,logwage,wage#  
attach(Wage)
#--------------------------maritl fit--------------------------------------------------
plot(maritl,wage)
levels(maritl) ##1.Never Married,2.Married,3.Widowed,4.Divorced,5.Separated
err=rep(0,10)
for(i in 1:10){
  fit=glm(maritl~poly(wage,i),data=Wage,family=binomial)
  err[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
which.min(err) #4th polynomial gives the best fit
plot(err,type="l")
contrasts(maritl) #Never Married 0000,Married 1000,Widowed 0100,Divorced 0010, Separated 0001
fit=glm(maritl~poly(wage,4),data=Wage,family=binomial)
pred=predict(fit,newdata=Wage,type="response")
plot(maritl,wage)
#-------------------------------------jobclass fit--------------------------------------------
plot(jobclass,wage)
levels(jobclass) #Industrial,Information
err=rep(0,20)
set.seed(1)
for(i in 1:20){
  fit=glm(jobclass~poly(wage,i),data=Wage,family=binomial)
  err[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
which.min(err) #11th polynomial gives the best fit
plot(err,type="l")
contrasts(jobclass) #Industrial=0,Information=1
set.seed(1)
fit=glm(jobclass~poly(wage,11),data=Wage,family=binomial)
pred=predict(fit,newdata=Wage,type="response")
predA=rep(0,nrow(Wage))
predA[pred>=0.5]='2. Information'
predA[pred<0.5]='1. Industrial'
table(predA,jobclass)
meanC=(916+856)/(916+600+628+856)  #Accuracy of the model is 59%
mean(predA==jobclass)
#-----------------------------------------------------------------------------------------------------------------------------------
#Both maritl and jobclass are categorical variable. 
#jobclass plot shows Information category has higher wage than Industrial on an average.
#marital plot shows highest to lowest wage is in order: Married,Widowed,Divorced,Separated,Never Married.
#jobclass prediction accuracy is around 59%. Easy to claculate because it has only two levels.
#marital prediction is not calculated here but can be computed by considering one category at a time and grouping all other to 0.
#-----------------------------------------------------------------------------------------------------------------------------------
