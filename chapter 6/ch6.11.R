library(MASS)
library(ISLR)
library(leaps)
library(glmnet)
library(pls)

names(Boston)#crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv
Hitters=na.omit(Hitters)
dim(Boston)#506x14

#--------------------------Best subset----------------------------------
sub.full=regsubsets(crim~.,data=Boston,nvmax=13)
summary(sub.full)
#-----------------------------------------------------------------------
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Boston),replace=TRUE,prob=c(400,106))
test=(!train)
#-----------------------------------------------------------------------
regfit.best=regsubsets(crim~.,data=Boston[train,],nvmax=13)
sub.test.modelX=model.matrix(crim~.,data=Boston[test,])
val.err=rep(NA,13)
for(i in 1:13){
  coefi=coef(regfit.best,id=i)  
  pred=sub.test.modelX[,names(coefi)]%*%coefi
  val.err[i]=mean((Boston$crim[test]-pred)^2)
}
minA=which.min(val.err) #12 with err=40.59,10 is very close with err=40.66
#calculating coeff on full dataset
regfit.best.full=regsubsets(crim~.,data=Boston,nvmax=14)
coef(regfit.best.full,10)
#-------------------------------Ridge---------------------------------------------
x=model.matrix(crim~.,data=Boston)[,-1]
y=Boston$crim
#----------------------------------------------------------------------------
ridge.fit=glmnet(x,y,alpha=0)
dim(coef(ridge.fit))  #14x10
set.seed(8)
ridge.cv=cv.glmnet(x[train,],y[train],alpha=0) #to choose best lambda
plot(ridge.cv)
bestlamRid=ridge.cv$lambda.min
bestlamRid #0.56937
ridge.predict=predict(ridge.fit,s=bestlamRid,newx = x[test,])
rid.err=mean((ridge.predict-y[test])^2)  #rid.err=39.24
#fitting on the full data to get coefficient estimate
ridge.fit.full=glmnet(x,y,alpha=0)
predict(ridge.fit.full,s=bestlamRid,type="coefficients")
#----------------------------Lasso--------------------------------------------
las.fit=glmnet(x,y,alpha=1)
dim(coef(ridge.fit))  #14x10
set.seed(2)
las.cv=cv.glmnet(x[train,],y[train],alpha=1) #to choose best lambda
plot(las.cv)
bestlamLas=las.cv$lambda.min
bestlamLas #0.05964
las.predict=predict(las.fit,s=bestlamLas,newx = x[test,])
las.err=mean((las.predict-y[test])^2)  #las.err=38.78
#fitting on the full data to get coefficient estimate
las.fit.full=glmnet(x,y,alpha=1)
predict(las.fit.full,s=bestlamLas,type="coefficients") #age and tax coeff are zero...
#----------------------------PCR-----------------------------------------------------------
set.seed(3)
pcr.fit=pcr(crim~.,data=Boston,scale=TRUE,validation="CV")
summary(pcr.fit) 
validationplot(pcr.fit,val.type="MSEP") 

set.seed(3)
pcr.fit.train=pcr(crim~.,data=Boston[train,],scale=TRUE,validation="CV")
summary(pcr.fit.train)
validationplot(pcr.fit.train,val.type="MSEP") #8comp RMSE=44.89,13comp RMSE=44.036

pcr.pred=predict(pcr.fit.train,x[test,],ncomp=8)
mean((pcr.pred-y[test])^2) #8comp MSE=42.21,13comp MSE=40.73

pcr.fit.all=pcr(y~x,scale=TRUE,ncomp=8)
summary(pcr.fit.all)
#---------------------------------(b)---------------------------------------------------------
#SUBSET MSE=40.66
#RIDGE MSE=39.24
#LASSO MSE= 38.78
#PCR MSE= 42.21 for 8 comp, 40.73 for all 13 comp, so M is not less than P (not useful)

#Model derived from Lasso seems to be best among all used...
#----------------------------------(c)-----------------------------------------
#Chosen model doesn not involve all of the features as some of the feature's coefficients in Lasso are zero(irrelevant).
#-------------------------------------------------------------------------------