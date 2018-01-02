#------------------------ch-6 Practice--------------------------

#-----------------Best subset selection-------------------------
library(MASS)
library(ISLR)
library(leaps) #regsubsets function
library(glmnet)
names(Hitters) #AtBat,Hits,HmRun,Runs,RBI,Walks,Years,CAtBat,CHits,CHmRun,
#CRuns,CRBI,CWalks,League,Division,PutOuts,Assists,Errors,Salary,NewLeague.
fix(Hitters)
attach(Hitters)
a=sum(is.na(Salary)) # Number of NA in Salary data=59-----------
dim(Hitters)  #322x20
Hitters=na.omit(Hitters)
dim(Hitters)  #263x20
library(leaps)
sub=regsubsets(Salary~.,data=Hitters,nvmax=19)
summ=summary(sub)
names(summ)
par(mfrow=c(2,2))
plot(summ$rss,xlab="number of variables",ylab="RSS",type="l")
which.min(summ$rss) #--19--
points(19,summ$rss[19],pch=20,col="red",cex=2)

plot(summ$adjr2,xlab="number of variables",ylab="Adjusted RSQ",type="l")
which.max(summ$adjr2) #--11--
points(11,summ$adjr2[11],pch=20,col="red",cex=2)

plot(summ$cp,xlab="number of variables",ylab="cp",type="l")
which.min(summ$cp) #--11--
points(10,summ$cp[10],pch=20,col="red",cex=2)

plot(summ$bic,xlab="number of variables",ylab="bic",type="l")
which.min(summ$bic) #--6--
points(6,summ$bic[6],pch=20,col="red",cex=2)

par(mfrow=c(1,1))
plot(sub,scale="r2")
plot(sub,scale="adjr2")
plot(sub,scale="Cp")
plot(sub,scale="bic")

coef(sub,6) #.......inbuilt coeff to give all the coeffs................
coef(sub,7)
coef(sub,5)
#-----------------------------------------------------------------------
#----------------Forward and Backward Stepwise Selection-----------------------------
subf=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(subf)
subb=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(subb)
#------------------------------------------------------------------------------
#------------------Using validation set----------------------------------------
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),replace=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
ModelXtest= model.matrix(Salary~.,data=Hitters[test,])
Errs=rep(0,19)
for(i in 1:19){
coeffi=coef(regfit.best,id=i)
estB=ModelXtest[,names(coeffi)]%*%coeffi
Errs[i]=mean((Hitters$Salary[test]-estB)^2) }
min(Errs)
which.min(Errs)  #10
#--------------------function to predict----------------------
predict.regsubsets =function(object,newdata,id){
form=as.formula (object$call [[2]])
mat=model.matrix (form ,newdata )
coefi =coef(object ,id=id)
xvars =names (coefi )
mat[,xvars ]%*% coefi
}
#------prediction based on full data set-----------------------------
regfit.fulldata=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.fulldata,id=10)
#-----------------Using cross validation-------------------------------------------------------------
set.seed(2)
k=10
fold=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix (NA ,k,19, dimnames =list(NULL , 1:19))

for(i in 1:k){
  regfitK=regsubsets(Salary~.,data=Hitters[fold!=k,],nvmax=19)
for(j in 1:19){
  predictK=predict.regsubsets(regfitK,Hitters[fold==i,],id=j)
cv.errors[i,j]=mean((Hitters$Salary[fold==i]-predictK)^2)
}}
cv.errors #10x19 matrix
finalM=apply(cv.errors,2,mean)
finalM
which.min(finalM) #14 variables
which.max(finalM)
plot(finalM,type='b')
points(14,finalM[19],pch=50,col="red",cex="2")
fulltainingK=regsubsets(Salary~.,data=Hitters,nvmax=19) #full data as training set
coef(fulltainingK,id=14) #14 variable betas from full training set.
#---------------------------------------------------------------------------------
#----------------------ridge regression-------------------------------------------
dim(Hitters)   #322x20
Hitters=na.omit(Hitters)
dim(Hitters)   #263x20
x=model.matrix(Salary~.,Hitters)[,-1] #remove the first col as it adds intercept automatically
y=Hitters$Salary
n=100
grid=10^seq(-2,10,length.out = n)
fit.ridge=glmnet(x,y,alpha=0,lambda = grid)
dim(coef(fit.ridge)) #20x100
fit.ridge$lambda[80] #2.65
coef(fit.ridge)[,80]
sum((coef(fit.ridge)[-1,80])^2) # 19577
fit.ridge$lambda[95] #0.04
coef(fit.ridge)[,95] 
sum((coef(fit.ridge)[-1,95])^2) # 18574
Prig=predict(fit.ridge,s=50,type="coefficients")

set.seed(10)
Train=sample(c(TRUE,FALSE),nrow(Hitters),replace = TRUE)
Test=!Train
trainR=glmnet(x[Train,],y[Train],alpha=0,lambda=grid)
predR=predict(trainR,s=grid,newx=x[Test,])
err=rep(0,100)
for(i in 1:n)
{err[i]=mean((y[Test]-predR[,i])^2)}
Ab=which.min(err) #9
plot(err,type="line")
points(9,err[9],pch=1,col="red")

#---------using cv.glmnet for crossvalidation------------------
set.seed(100)
tr=cv.glmnet(x[Train,],y[Train],alpha=0)
plot(tr)
minlambda=tr$lambda.min #460
trp=predict(trainR,s=minlambda,newx=x[Test,])
MSER=mean((y[Test]-trp)^2)  # 128075
#--------------testing on full data set------------------------
out=glmnet(x,y,alpha=0,lambda=minlambda)
trpfull=predict(out,s=minlambda,newx=x[test ,])
MSEFULL=mean((y[test]-trpfull)^2) #110223
#-------------------------Lasso-------------------------------------
trainRL=glmnet(x[Train,],y[Train],alpha=1,lambda=grid)
set.seed(100)
trL=cv.glmnet(x[Train,],y[Train],alpha=1)
plot(trL)
coef(trL) #Hits,RBI,CRBI are non-zeros
minlambdaL=trL$lambda.min #1.694
outL=glmnet(x,y,alpha=1,lambda=grid)
trpfullL=predict(outL,s=minlambdaL,type="coefficients")
trpfullL[1:20,]
#-------------------------------------------------------------------

