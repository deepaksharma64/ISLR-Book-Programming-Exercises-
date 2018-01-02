#--------------ch-6 Problem 8-------------------------------------------------
library(MASS)
library(ISLR)
library(leaps)
library(glmnet)
#---------------------(a)-----------------------------------------------------
enoise=rnorm(100)
set.seed(1)
X=rnorm(100)
#---------------------(b)-----------------------------------------------------
b0=2
b1=5
b2=8
b3=3
Y=b0+b1*X+b2*X^2+b3*X^3+enoise
#--------------------(c)------------------------------------------------------
dataSim=data.frame(Y,X,X^2,X^3,X^4,X^5,X^6,X^7,X^8,X^9,X^10)
sim.sub.fit=regsubsets(Y~.,data=dataSim,nvmax=10)
names(summary(sim.sub.fit)) 
coefficients(sim.sub.fit,id=3) #intercept,X,X.2,X.3=1.99,4.99,8,2.99 respectively
ADJR2=summary(sim.sub.fit)$adjr2  # min ([1]>X3) 
CP=summary(sim.sub.fit)$cp   # min ([4]>X,X2,X3,X5) 
BIC=summary(sim.sub.fit)$bic  # min ([3]>X,X2,X3)
plot(ADJR2)
plot(CP)
plot(BIC)
coef(sim.sub.fit,id=1) #X.3===3.087,Intercept=637
coef(sim.sub.fit,id=3) #X===2.0,X.2===7.999,X.3===3.000,Intercept=2.0
coef(sim.sub.fit,id=4) #X===2.0,X.2=7.999,X.3=2.999,X.5=4.036E-7,Intercept=2.0
#----------------------------(d)---forward-------------------------------------------
sim.sub.fit.F=regsubsets(Y~.,data=dataSim,nvmax=10,method="forward")
ADJR2F=summary(sim.sub.fit.F)$adjr2  # min ([1]>X3) 
CPF=summary(sim.sub.fit.F)$cp   # min ([4]>X,X2,X3,X5) 
BICF=summary(sim.sub.fit.F)$bic  # min ([3]>X,X2,X3) 
#----------------------------(d)---backward-------------------------------------------
sim.sub.fit.B=regsubsets(Y~.,data=dataSim,nvmax=10,method="backward")
ADJR2B=summary(sim.sub.fit.B)$adjr2  # min ([1]>X3) 
CPB=summary(sim.sub.fit.B)$cp   # min ([4]>X,X2,X3,X9) 
BICB=summary(sim.sub.fit.B)$bic  # min ([3]>X,X2,X3) 
#All three are almost similar except in Backward selection for cp, X.9 is selected.
#-----------------------------(e)---lasso--------------------------------------------
Xdata=matrix(c(X,X^2,X^3,X^4,X^5,X^6,X^7,X^8,X^9,X^10),nrow=100,byrow=FALSE)
Ydata=matrix(Y)
lasso.Train.cv=cv.glmnet(Xdata,Ydata,nfold=10,alpha=1)
lasp=plot(lasso.Train.cv) #cv error vs lambda plot
coef(lasso.Train.cv) #first 3 are V0=176.8,V1=1.01,V2=5.472,V3=2.8975 rest are 0.....
minLam=lasso.Train.cv$lambda.min  #217.05
predict(lasso.Train.cv, s=minLam, type="coefficients")
#coeff are V0=193,V1=0,V2=6.40,V3=2.94.Coefficients are chosen appropriately by lasso.Their coeff are also colse to real assumed values of b. 
#-----------------------------(f)-----------------------------------------------------------
b01=5
b7=10
Y1<-b01+b7*(X^7)+enoise

dataSim1=data.frame(Y1,X,X^2,X^3,X^4,X^5,X^6,X^7,X^8,X^9,X^10)
sim.sub.fit1=regsubsets(Y1~.,data=dataSim1,nvmax=10)
names(summary(sim.sub.fit1)) 
coefficients(sim.sub.fit1,id=1:10) #predicts coeff correctly.
ADJR2=summary(sim.sub.fit1)$adjr2  
CP=summary(sim.sub.fit1)$cp  
BIC=summary(sim.sub.fit1)$bic  

lasso.Train.cv1=cv.glmnet(Xdata,Y1,alpha=1)
minLam1=lasso.Train.cv1$lambda.min  #193904141
predict(lasso.Train.cv1, s=minLam1, type="coefficients")
#both the models estimates predictors accurately.  
#-------------------------------------------------------------------------------------

