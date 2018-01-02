#-------------------------------------ch7---(10)------------------------------------
library(ISLR)
library(splines)
library(gam)
library(MASS)
library(boot)
library(leaps)
dim(College) #777x18
#---------------------------------------(a)-----------------------------------------
set.seed(1)
ind=sample(c(TRUE,FALSE),777,replace=TRUE,prob = c(500,277))
Train=College[ind,] #498x18
Test=College[!ind,] #279x18
attach(College)
Test.mat=model.matrix(Outstate~.,data=Test)
reg.fit=regsubsets(Outstate~.,data=Train,nvmax=(ncol(College)-1),method='forward')
names(summary(reg.fit))
err=rep(0,(ncol(College)-1))
for(i in 1:(ncol(College)-1)){
 coefi=coefficients(reg.fit,id=i)
 err[i]=mean((Test$Outstate-Test.mat[,names(coefi)]%*%coefi)^2)
}
which.min(err) #17 
err  #value for 17th =4300105, 6th= 4477843
plot(err,type='l') #shows after 6 predictors errors are very similar so we can choose 6 variables
adjr2s=summary(reg.fit)$adjr2
plot(adjr2s)
which.max(adjr2s) #12
cps=summary(reg.fit)$cp
plot(cps)
which.min(cps) #11
bics=summary(reg.fit)$bic
plot(bics)
which.min(bics)
#08
coefficients(reg.fit,6)
summary(reg.fit)$outmat[6,]
#Based on cv.glm we can pic 10 predictors.
#---------------------------------------(b)----------------------------------------------
plot(College)
fit=gam(Outstate~s(Room.Board,3)+s(Terminal,3)+s(perc.alumni,3)+s(Expend,3)+s(Grad.Rate,3)+Private,data=Train)
summary(fit)
plot.gam(fit,se=TRUE,col="blue") 
#plots show that all the predictor's fits are not linear as we assumed in the part (a)
#----------------------------------------(c)-----------------------------------------------
pred=predict(fit,newdata = College)
err=mean((pred-College$Outstate)^2) #err=3477588 for 6 variables.This err is significantly less than err in part (a)
#----------------------------------------(d)-----------------------------------------------
plot.gam(fit,se=TRUE,col="blue") #Expend,Terminal predictors are clearly non linear.
#------------------------------------------------------------------------------------------