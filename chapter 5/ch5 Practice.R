#-----------------ch5 Practice---------------------------------------------------------
#--------------------------------------------------------------------------------------
library(ISLR)
library(class)
library(MASS)
dim(Auto)    #--392x9---
set.seed(1)
train=sample(392,196)
attach(Auto)
linA=lm(mpg~horsepower,data=Auto,subset=train)
A1=predict(linA,Auto[-train])
MSE1= mean((Auto[-train]$mpg-A1)^2)  #MSE 23.965
#--------------------------------------------------------------------------------------
linB=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
B1=predict(linB,Auto[-train])
MSE2= mean((Auto[-train]$mpg-B1)^2)  #MSE 19.056
#--------------------------------------------------------------------------------------
linC=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
C1=predict(linC,Auto[-train])
MSE3= mean((Auto[-train]$mpg-C1)^2)  #MSE 19.028
#--------------------------------------------------------------------------------------
#----------------------------LOOCV-----------------------------------------------------
library(boot)
set.seed(5)
cv.err=rep(0,5)
for(i in 1:5){
lin.fit=glm(mpg~poly(horsepower,i),data=Auto)
cv.err[i]=cv.glm(Auto,lin.fit)$delta[1]
}
cv.err
#---------------------------------------------------------------------------------------
set.seed(10)
cv.err=rep(0,10)
for(i in 1:10){
  lin.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.err[i]=cv.glm(Auto,lin.fit,K=10)$delta[1]
}
cv.err
#----------------------------BootStrap--------------------------------------------------
setF=function(data,index){
X=data$X[index]
Y=data$Y[index]
return((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}
setF(Portfolio,1:100)   #0.575
set.seed(1)
setF(Portfolio,sample(100,100,replace=T)) #0.5963 this can be repeated mant times.

boot(Portfolio,setF,R=1000) #If this command is used,top two lines are not needed.
#----------------------------------simple------------------------------------------------
dim(Auto)   #dim 392x9
betas=function(data,index){
fit.A=lm(mpg~horsepower,data=Auto,subset=index)
return(coef(fit.A))
}
betas(Auto,1:392)    #39.9358610  -0.1578447 
set.seed(5)
betas(Auto,sample(1:392,replace =T))  #40.0418687  -0.1583257 can be repeated several times.
boot(Auto,betas,1000)  #39.9358610  -0.1578447 above statement is not needed.
#std errors are 0.825611679 0.007053179
summary(lm(mpg~horsepower,data=Auto)) #std errors 0.717499 and 0.006446
#---------------------------------quadratic --------------------------------------------------
dim(Auto)   #dim 392x9
betas=function(data,index){
fit.B=lm(mpg~poly(horsepower,2),data=data,subset=index)
return(coef(fit.B))
}
betas(Auto,1:392)   
set.seed(5)
betas(Auto,sample(1:392,replace =T)) 
boot(Auto,betas,1000) #stderrors=0.2217,3.6204,4.2764
summary(lm(mpg~poly(horsepower,2),data=Auto)) #stderrors=0.2209,4,3739,4.3739
#---------------------------------------------------------------------------------------------
