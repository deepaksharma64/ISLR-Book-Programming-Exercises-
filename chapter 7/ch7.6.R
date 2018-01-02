#--------------------------------------------ch-7 ex6---------------------------
library(ISLR)
library(gam)
library(boot)
library(MASS)
dim(Wage) #3000x12
names(Wage)#year,age,sex,maritl,race,education,region,jobclass,health,health_ins,logwage,wage#  


err=rep(0,10)
for(i in 1:10){
  fit=glm(wage~poly(age,i),data=Wage)
  err[i]=cv.glm(Wage,fit,K=10)$delta[1]
}
plot(err,type="b")
#4th polynomial gives minimum error.....
#ANNOVA TEST------------------------
fit.1=lm(wage~poly(age,1),data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
fit.6=lm(wage~poly(age,6),data=Wage)
fit.7=lm(wage~poly(age,7),data=Wage)
fit.8=lm(wage~poly(age,8),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6,fit.7,fit.8)
#Annova test suggest polynomial 3 or 4 while crosss-validation suggests 4th polynomial.
age.g=range(age)
age.grid=seq(from=age.g[1],to=age.g[2])

fit=glm(wage~poly(age,4),data=Wage)
pred=predict(fit,newdata=list(age=age.grid))
predA=predict(fit.3,newdata=list(age=age.grid))
attach(Wage)
plot(age,wage)
lines(age.grid,pred,col="red") #polynomial 4 fit 
lines(age.grid,predA,col="blue") #Annova fit for polynomial=4
#-------------------------------------(b)--------------------------------------------
err=rep(1,25)
set.seed(10)
for(i in 2:26){
  Wage$bucket=cut(Wage$age,i)
  fit=glm(wage~bucket,data=Wage)
  err[i-1]=cv.glm(Wage,fit,K=10)$delta[1]
}
plot(err) #min err is for cut=15
which.min(err)
fita=glm(wage~cut(age,15),data=Wage)
predC=predict(fita,newdata=list(age=age.grid))
plot(age,wage)
lines(age.grid,predC,col="pink",lwd=5)
#-----------------------------------------------------------------------------