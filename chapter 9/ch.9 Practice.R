#---------------------------------ch-9---Practice------------------
library(ISLR)
library(e1071)
library(ROCR)
seet.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y)) #not linearly separable--------------------------

#support vector classifer----------------
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
plot(svmfit,dat)
svmfit$index #no of support vector 12

summary(svmfit)
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
summary(svmfit)  #no of support vector 16

#tune function for crossvalidation
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",range=list(cost=
          c(0.001,0.01,0.1,1,5,10,100)))  
summary  #best performance(error): 0.3 #best parameter 0.1
bestmod=tune.out$best.model
summary(bestmod)
#predict function to predict test set

xtest=matrix(rnorm(20*2),ncol=2)
ytest=sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
#what ifcost=0.01
svmfit=svm(y~.,data=dat,kernel="linear",cost=0.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred,truth=testdat$y)
# Case when two classes are linearly separable------------------
x[y==1,]=x[y==1,]+0.5
plot(x,col=(y+5)/2,pch=19)
dat=data.frame(x=x,y=as.factor(y))

svmfit=svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat) #no. fo support vectors=3

svmfit=svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit) #no. of support vectors=6
plot(svmfit,dat)
#---------------Support Vector Machine--------
#kernel="polynomail", degree=???
#kernel="radial",  gamma=????

set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x,col=y)

train=sample(200,100)
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])
summary(svmfit)
#increasing the value of cost will reduce the number of trainning error
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
#CV using tune( to select the best choice of cost)

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
              gamma=c(0.5,1,2,3,4)))
summary(tune.out)
#best parameters: cost=1,gamma=2,best performance=0.12
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,]))
#10% of test observation are misclassified by this SVM

#-------------------ROC Curves--------------------------
rocplot=function(pred,truth,...){
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...) 
  }  #Function to plot ROC 

svmfit.opt=svm(y~.,data=dat[train,],kernel="radial",
               gamma=2,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],
        decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")

# increasing gamma will produce more accurate results

svmfit.flex=svm(y~.,data=dat[train,],kernel="radial",
               gamma=50,cost=1,decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],
                          decision.values=TRUE))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
#On test data---------------------------------------------
fitted=attributes(predict(svmfit.opt,dat[-train,],
                          decision.values=TRUE))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],
                          decision.values=TRUE))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red") 
#SVM With multiple class----------------------------------------
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~.,data=dat,kernel="radial",gamma=1,cost=1)
plot(svmfit,dat)
#-----------Application to Gene Expression Data---------
names(Khan) #"xtrain","xtest","ytrain","ytest"
dim(Khan$xtrain) #63x2308
dim(Khan$xtest) #20x2308
length(Khan$ytrain) #63x1
length(Khan$ytest) #20x1

dat=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y~.,data=dat,kernel="linear",cost=10)
summary(out)
table(out$fitted,dat$y) #there are no training errors.

dat.te=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te=predict(out,newdata=dat.te)
table(pred.te,dat.te$y) #there are only two errors.
#--------------------------------------------------------












