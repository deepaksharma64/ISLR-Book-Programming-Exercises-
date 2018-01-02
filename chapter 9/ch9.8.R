#-----------------------------ch-9----(8)-------------------------
library(ISLR)
library(e1071)
library(dplyr)
dim(OJ) #1070x18
#---------------------------------(a)-----------------------------
set.seed(1)
tr=sample(1070,800)
train=OJ[tr,]  #800x18
test=OJ[-tr,]  #270x18
#---------------------------------(b)-----------------------------
set.seed(1)
svm.linear=svm(Purchase~.,data=train,kernel="linear",cost=0.01)
summary(svm.linear)
#default gamma is 0.055,SVM-kernel=linear
#number of support vectors: 432 (215,217)
#---------------------------------(c)-----------------------------
Yp=predict(svm.linear,train)
trainerr= 1-mean(Yp==train$Purchase)  #trainerr = 0.16
Yt=predict(svm.linear,test)
testerr= 1-mean(Yt==test$Purchase)  #testerr = 0.18
#--------------------------------(d)------------------------------
set.seed(5)
tune.out=tune(svm,Purchase~.,data=train,kernel="linear",range=list(cost=c(0.01,0.01,0.1,1,5,10)))  
summary(tune.out)  #best cost=5

bestmod=tune.out$best.model
summary(bestmod) #best is cost=100 gamma=2
#-------------------------------(e)-------------------------------
ytrain=predict(bestmod,train)
1-mean(ytrain==train$Purchase) #trainerr = 0.1562

ytest=predict(bestmod,test)
1-mean(ytest==test$Purchase) #testerr = 0.1777
#---------------------------------(f)-----------------------------
set.seed(1)
svm.radial=svm(Purchase~.,data=train,kernel="radial",cost=0.01)
summary(svm.radial)

Yp=predict(svm.radial,train)
trainerr= 1-mean(Yp==train$Purchase)  #trainerr = 0.38
Yt=predict(svm.radial,test)
testerr= 1-mean(Yt==test$Purchase)  #testerr = 0.0.41

set.seed(5)
tune.out=tune(svm,Purchase~.,data=train,kernel="radial",range=list(cost=c(0.01,0.01,0.1,1,5,10)))  
summary(tune.out)  #best cost=1

bestmod=tune.out$best.model
summary(bestmod) #best is cost=1 gamma=0.055

ytrain=predict(bestmod,train)
1-mean(ytrain==train$Purchase) #trainerr = 0.145

ytest=predict(bestmod,test)
1-mean(ytest==test$Purchase) #testerr = 0.1703
#----------------------------------(g)------------------------------
set.seed(1)
svm.polynomial=svm(Purchase~.,data=train,kernel="polynomial",cost=0.01)
summary(svm.polynomial)

Yp=predict(svm.polynomial,train)
trainerr= 1-mean(Yp==train$Purchase)  #trainerr = 0.36
Yt=predict(svm.radial,test)
testerr= 1-mean(Yt==test$Purchase)  #testerr = 0.41

set.seed(5)
tune.out=tune(svm,Purchase~.,data=train,kernel="polynomial",degree=2,range=list(cost=c(0.01,0.01,0.1,1,5,10)))  
summary(tune.out)  #best cost=10

bestmod=tune.out$best.model
summary(bestmod) #best is cost=10 

ytrain=predict(bestmod,train)
1-mean(ytrain==train$Purchase) #trainerr = 0.145

ytest=predict(bestmod,test)
1-mean(ytest==test$Purchase) #testerr = 0.185
#----------------------------(h)---------------------------------
#test errs: radial =0.1703,linear=0.1777,polynomial=0.185
#best performance is given by radial.
#----------------------------------------------------------------