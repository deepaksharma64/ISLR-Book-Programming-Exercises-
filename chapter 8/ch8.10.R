#--------------------------ch8-----(10)------------
library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library (pls)
#----------------------(a)--------------------------
dim(Hitters) #322x20
names(Hitters)
Hitters=na.omit(Hitters) #263x20
head(Hitters$Salary) #before log transform: 475,480,500,91.5,750,70
Hitters$Salary=log(Hitters$Salary)
head(Hitters$Salary) #After log transformation 6.16,6.17,6.21,4.51,6.62.4.24
#----------------------(b)--------------------------
hitters.train=Hitters[1:200,]
hitters.test=Hitters[201:nrow(Hitters),]
#-----------------------(c)-------------------------
grid.shrinkage=seq(from=.0001,to=1,by=((1-0.0001)/10))
mse.train=rep(0,11)
mse.test=rep(0,11)
set.seed(1)
for(i in 1:11){
boost.hitters=gbm(Salary~.,data=hitters.train,distribution = "gaussian",n.trees = 1000,
                 interaction.depth=4,shrinkage=grid.shrinkage[i],verbose = F)

mse.train[i]=mean((boost.hitters$train.error)^2) #training error
yhat.boost=predict(boost.hitters,newdata=hitters.test,n.trees=1000) #test error
mse.test[i]=mean((yhat.boost-hitters.test$Salary)^2) #test MSE=0.350 from boosting
}
plot(grid.shrinkage,mse.train)
which.min(mse.train) #8th i.e shrinkage value correspinding to grid.shrinkage[8]=0.70003. MSE is 0.0003890605
#--------------------------(d)---------------------
plot(grid.shrinkage,mse.test)
which.min(mse.test) #3rd i.e shrinkage value correspinding to grid.shrinkage[3]=0.20008. MSE is 0.2872925
#--------------------------(e)---------------------
#PCR from chapter-6
set.seed (2)
pcr.fit=pcr(Salary~.,data=hitters.train,scale=TRUE,validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP") #minimum for M=16 but M=8 is very close,so we choose M=8
pcr.pred=predict (pcr.fit ,hitters.test, ncomp =8)
mean((pcr.pred -hitters.test$Salary)^2)  #test MSE=0.474 computed from PCA
#Multiple linear Regression fron chapter-3
lm.fit =lm(Salary~.,data=hitters.train)
Xmodel=model.matrix(Salary~.,data=hitters.test) #dim 63x20
coefM=as.matrix(lm.fit$coefficients) #dim=20x1
yhat=Xmodel%*%coefM  #63x1
mse.test=mean((hitters.test$Salary-yhat)^2) #test MSE=0.491 compted from multiple linear regression
#--------------------------(f)---------------------
summary(boost.hitters)  #CAtBat and CRuns has the most important predictors.
par(mfrow=c(1,2))
plot(boost.hitters,i="CAtBat")
plot(boost.hitters,i="CRuns")
#--------------------------(g)---------------------
set.seed(1)
bag.hitters=randomForest(Salary~.,data=hitters.train,mtry=19,importance=TRUE)
yhat.bag=predict(bag.hitters,newdata=hitters.test)
mean((yhat.bag-hitters.test$Salary)^2) #test MSE = 0.228 computed from bagging.
#bagging is giving lower test mse than boosting in this case.
#--------------------------------------------------




