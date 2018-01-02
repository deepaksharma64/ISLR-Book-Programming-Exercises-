#-------------------------------------ch7--(9)--------------------------------------------
library(ISLR)
library(splines)
library(gam)
library(MASS)
library(boot)

dim(Boston) #506 14
names(Boston) #We use "dis" and "nox" predictor for this variable
# "dis" -predictor- the weighted mean of distances to five Boston employment centers
# "nox" -response- nitrogen oxides concentration in parts per 10 million
attach(Boston)
plot(dis,nox)
#---------------------------------------(a)-----------------------------------------------
fit1=glm(nox~poly(dis,3),data=Boston)
ran=range(dis)
dis.grid=seq(ran[1],ran[2])
pred=predict(fit1,newdata=data.frame(dis=dis.grid))
plot(dis,nox)
lines(dis.grid,pred,col="red")
#---------------------------------(b)----------------------------------------------
R2=rep(0,10)
for(i in 1:10){
  fit=lm(nox~poly(dis,i),data=Boston)
  R2[i]=summary(fit)$r.squared
}
#R2 values are: 0.5917150 0.6998562 0.7147737 0.7149397 0.7175487 0.7230100 0.7272533 0.7292963 0.7296354 [10] 0.7298064
#---------------------------------(c)------------------------------------------------
err=rep(0,10)
set.seed(1)
for(i in 1:10){
  fit=glm(nox~poly(dis,i),data=Boston)
  err[i]=cv.glm(Boston,fit,K=10)$delta[1]
}
plot(err,type = "line")
which.min(err) #4th polynomial
#-----------------------------------(d)-----------------------------------------------
fit1=lm(nox~bs(dis,df=4),data=Boston)
ran=range(dis)
dis.grid=seq(ran[1],ran[2])
pred=predict(fit1,newdata=data.frame(dis=dis.grid))
plot(dis,nox)
lines(dis.grid,pred,col="red")
#-----------------------------------(e)---------------------------------------------------
RSS=rep(0,17)
for(i in 3:20){
  fit=lm(nox~bs(dis,df=i),data=Boston)
  RSS[i-2]=sum((fit$residuals)^2)
} #used df command to choose equally spaced knots. 
plot(RSS) #As expected RSS goes down with increasing degree of fredom. 
#-----------------------------------(f)---------------------------------------------------
set.seed(3)
cverr=rep(0,17)
for(i in 3:20){
  fit=glm(nox~bs(dis,df=i),data=Boston)
  cverr[i-2]=cv.glm(Boston,fit,K=10)$delta[1]
} #used df command to choose equally spaced knots. 
which.min(cverr) #df=10 gives the minimum cv.error and hence the best modelfit
#------------------------------------------------------------------------------------------