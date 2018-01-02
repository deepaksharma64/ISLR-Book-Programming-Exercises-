#-----ch-9-------(5)----------------------
library(ISLR)
library(e1071)
#-------(a)-------------
set.seed(1)
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*((x1^2-x2^2)>0)
dat=data.frame(y=as.factor(y),x1=x1,x2=x2)
#-------(b)-------------
plot(x1,x2,col=y+1)
#-------(c)-------------
fit.logistic=glm(y~.,data = dat,family=binomial)
summary(fit.logistic)
#--------(d)------------
tr=sample(500,350)
train=dat[tr,]
test=dat[-tr,]
pre=predict(fit.logistic,newdata = train,type="response")
pred=ifelse(pre>=0.5,1,0)
plot(train$x1,train$x2,col=pred+1) #Clearly decision bounday is linear
table(pred,train$y) 
mean(pred==train$y) #55.7%
#---------(e) and (f)-----------
fit.logistic1=glm(y~poly(x1,2)+poly(x2,2),data = dat,family=binomial)
pre=predict(fit.logistic1,newdata = train,type="response")
pred=ifelse(pre>=0.5,1,0)
plot(train$x1,train$x2,col=pred+1) #Decision bounday is perfect
table(pred,train$y) #Accuracy is 100% as expected since fitted and actual data have same degree
mean(pred==train$y) 

fit.logistic2=glm(y~poly(x1,2)+x2,data = dat,family=binomial)
pre=predict(fit.logistic2,newdata = train,type="response")
pred=ifelse(pre>=0.5,1,0)
plot(train$x1,train$x2,col=pred+1) #Decision bounday is not linear
table(pred,train$y) #Accuracy is (139+121)/(139+121+35+55) =0.74
mean(pred==train$y) #0.74
#-----------(g)------------------linear kirnel---
set.seed(1)
tune.out1=tune(svm,y~.,data=dat,kernel="linear",range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))  
summary(tune.out1)
bestmod1=tune.out1$best.model
summary(bestmod1) #best is cost=.001

ypred1=predict(bestmod1,train)
table(ypred1,train$y) 
mean(ypred1==train$y) #52.28%
svmfit1=svm(y~.,data=train,kernel="linear",cost=.001)
plot(svmfit1,train)
#-----------(h)------------------radial kernel---
set.seed(1)
tune.out2=tune(svm,y~.,data=dat,kernel="radial",range=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=c(0.5,1,2,3,4)))  
summary(tune.out2) 
bestmod2=tune.out2$best.model
summary(bestmod2) #best is cost=100 gamma=2

ypred2=predict(bestmod2,train)
table(ypred2,train$y) 
mean(ypred2==train$y) #99.42
svmfit2=svm(y~.,data=train,kernel="radial",gamma=2,cost=100)
plot(svmfit2,train)
#--------------(i)---------------------------------
#radial kernal performs best,linear kernal and linear logistic regression have similar accuracy of about 50% 
#Polynomial logistic performance varies according to assumption made for the polynomial degree.
#--------------------------------------------------
                  