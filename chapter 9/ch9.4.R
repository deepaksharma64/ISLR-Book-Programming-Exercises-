library(ISLR)
library(e1071)
set.seed(1)
y=sample(c(0,1),150,replace=T,prob = c(75,75))
x=matrix(rnorm(150*2),ncol=2)
x[y==1,]=(x[y==1,])^2+.5
datM=data.frame(x=x,y=as.factor(y))
attach(datM)
plot(x,col=y+1)
train=datM[1:100,]
test=datM[101:150,]
#support vector classifier-----------------------------------
set.seed(1)
tune.out=tune(svm,y~.,data=train,kernel="linear",range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))  
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod) #best is cost=1
ypred=predict(bestmod,train)
table(predict=ypred,truth=train$y) #trainning error = (37+52)/(37+52+0+11) =0.89
ypred=predict(bestmod,test)
table(predict=ypred,truth=test$y) #test error =(19+24)/(7+0+19+24)= 0.86
svmfit=svm(y~.,data=train,kernel="linear",cost=1)
plot(svmfit,train)
#Radial kernel-------------------------------------------
set.seed(1)
tune.out=tune(svm,y~.,data=train,kernel="radial",range=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=c(0.5,1,2,3,4)))  
summary(tune.out) 
bestmod=tune.out$best.model
summary(bestmod) #best is cost=5 gamma=2
ypred=predict(bestmod,train)
table(predict=ypred,truth=train$y) #trainning error = (42+52)/(42+52+6+0) =0.94
ypred=predict(bestmod,test)
table(predict=ypred,truth=test$y) #test error =(20+24)/(6+0+20+24)= 0.88
svmfit=svm(y~.,data=train,kernel="radial",gamma=2,cost=5)
plot(svmfit,train)
#radial kernalperforms better on both training data set and test data set.
#--------------------------------------------------------------


