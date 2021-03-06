#----------------------------------ch-7--(11)--------------------------------------
library(ISLR)
library(splines)
library(gam)
library(MASS)
library(boot)
library(leaps)
#----------------------------------------(a)---------------------------------------
set.seed(1)
X1=rnorm(100)*10
X2=rnorm(100)*10
set.seed(2)
e=rnorm(100)
b0=8
b1=5
b2=-2
Y=b0+(b1*X1)+(b2*X2)+e
toyM=as.matrix(cbind(Y,X1,X2),nrow=100,ncol=3)
toy=data.frame(toyM)
#----------------------------------------(b)----------------------------------------
beta1=1
#----------------------------------------(c)----------------------------------------
a1=Y-beta1*X1 #set beta1 constant
beta2=lm(a1~X2)$coeff[2]
#----------------------------------------(d)----------------------------------------
a2=Y-beta2*X2
beta1=lm(a2~X1)$coeff[2]
#----------------------------------------(e)----------------------------------------
beta1c=rep(0,1000)
beta2c=rep(0,1000)
beta0=rep(0,1000)
a=rep(0,100)
beta1=1
beta2=0
for(i in 1:1000){            
  a=Y-(beta1*X1)
  beta2=lm(a~X2)$coeff[2]
  beta2c[i]=beta2
  a=Y-beta2*X2
  beta1=lm(a~X1)$coeff[2]
  beta1c[i]=beta1
  beta0[i]=lm(a~X1)$coeff[1]
}
Values=seq(1:1000)
data1=as.matrix(cbind(Values,beta0,beta1c,beta2c),nrow=1000,ncol=4)
df=data.frame(data1)
plot(df$Values,df$beta0,col="red",ylim=c(-3,10),lwd=1)
lines(df$Values,df$beta1c,col="blue",lwd=7)
lines(df$Values,df$beta2c,col="black",lwd=7)
#---------------------------------------(f)-----------------------------------------
fit=lm(Y~X1+X2)
beta0M=fit$coeff[1]
beta1M=fit$coeff[2]
beta2M=fit$coeff[3]
abline(h=beta0M,lwd=3)
abline(h=beta1M,lwd=3)
abline(h=beta2M,lwd=3)
#Clearly the values of coefficients are equal in e and f.
#----------------------------------------(g)----------------------------------------
#It took just one iteration to get quite accurate prediction.
#-----------------------------------------------------------------------------------
