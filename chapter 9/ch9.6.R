#---------------------------ch-9----(6)------------------------------------
library(ISLR)
library(e1071)
#------------------------(a)----------------
set.seed(10)
x1=runif(300)-0.5
x2=runif(300)-0.5
y=1*((x1-x2)>0)
dat=data.frame(y=as.factor(y),x1=x1,x2=x2)
plot(x1,x2,col=y+1)
dat=data.frame(y=as.factor(y),x1=x1,x2=x2)
set.seed(1)
trt=sample(300,200)
train=dat[trt,]  #training sample 350x3
test=dat[-trt,]
#------------------------(b)-----------------
set.seed(5)
tune.out=tune(svm,y~.,data=train,kernel="linear",range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))  
summary(tune.out)
#clearly as cost value goes up CV error decreases.
cr=c(0.001,0.01,0.1,1,5,10,100)
trainEr=rep(100,length(cr))
for(i in 1:length(cr)){
svmfit=svm(y~.,data=train,kernel="linear",cost=cr[i],scale=FALSE)
ypred=predict(svmfit,train)
trainEr[i]=1-(mean(ypred==train$y)) 
}
trainEr #% of misclassified training error decreases with increasing cost.
#-------------------------(c)-----------------
testEr=rep(100,length(cr))
for(i in 1:length(cr)){
  svmfit=svm(y~.,data=train,kernel="linear",cost=cr[i],scale=FALSE)
  ypred=predict(svmfit,newdata=test)
  testEr[i]=1-(mean(ypred==test$y)) 
} 
testEr #test error is lowest for cost 1 and after that it increases for increasing cost suggesting overfitting.
#-------------------------(d)---------------------------------
#clearly as cost value goes up CV error decreases.
#% of misclassified training error decreases with increasing cost.
#% of misclassified test error does not decrease with increasing cost for all values of cost. 