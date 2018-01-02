#--------------------------ch-7--(12)-----------------------------------
library(ISLR)
library(splines)
library(gam)
library(MASS)
library(boot)
library(leaps)

set.seed(1)
e=rnorm(1000)
set.seed(2)
toyM=matrix(rnorm(100000),nrow=1000,ncol=100)
B=matrix(seq(1:101),nrow=101)
Y=B[1,1]+toyM%*%B[2:101,1]+e #coeff assumed are 1,2,3,4...100 of 100 predictors 
toyMa=cbind(Y,toyM)
toy=data.frame(toyMa)
#multiple linear regression---------
fit=lm(X1~.,data=toy)
AX=model.matrix(X1~.,data=toy)
coefM=matrix(coefficients(fit)) #coefficient by multiple linear regression.
errMR=mean((Y-AX%*%coefM)^2)  #0.957
#linear regression by backfitting---
beta=matrix(rep(0,101))
err=rep(0,100)
for(i in 1:100){
  for(j in 1:100){
    betaS=matrix(beta[-c(1,j+1),])
    a=Y-toyM[,-j]%*%betaS
    beta[j+1,]=lm(a~toy[,j+1])$coef[2]
    beta[1,]=lm(a~toy[,j+1])$coef[1]
  }
err[i]=mean((Y-(AX%*%beta))^2)
}
plot(err,ylim=c(2,0.5))
err[1:10]
#It took just less than 10 iteration for err to be almost equal to errMR.
#------------------------------------------------------------------------
