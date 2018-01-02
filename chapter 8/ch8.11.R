#--------------------------ch8-----(11)------------
library(ISLR)
library(tree)
library(randomForest)
library(gbm)
#----------------------(a)--------------------------
attach(Caravan)
Caravan$Purchase=ifelse(Purchase=="No",0,1)
dim(Caravan) #5822x86
caravan.train=Caravan[1:1000,]
caravan.test=Caravan[1001:nrow(Caravan),]
#-----------------------(b)-------------------------

set.seed(1)
boost.caravan=gbm(Purchase~.,data=caravan.train,distribution="bernoulli",n.trees = 1000,
              shrinkage=0.01)
summary(boost.caravan)
par(mfrow=c(2,2))1
plot(boost.caravan,i="PPERSAUT")
plot(boost.caravan,i="MKOOPKLA")
plot(boost.caravan,i="MOPLHOOG")
plot(boost.caravan,i="MBERMIDD")
#Most important variables seems to be "PPERSAUT" then "MKOOPKLA" then "MOPLHOOG" then "MBERMIDD"
#-----------------------(c)------------------------
yhat.boost=predict(boost.caravan,newdata=caravan.test,n.trees=1000,type="response")
yhat.binary=ifelse(yhat.boost<0.2,0,1)
table(yhat.binary,caravan.test$Purchase)  # "No" is 0 and "Yes" is 1. 
(4410+33)/(4410+33+256+123) #Test Accuracy is 92.14%
# 33 out of (123+33=156) were predicted correctly to make the purchase i.e 33/156=0.211
#------------------logical Regression--------------
logical.caravan=glm(Purchase~.,data=caravan.train,family=binomial)
yhat.logical=predict(logical.caravan,newdata=caravan.test,type="response")
yhat.logic=ifelse(yhat.logical<0.2,0,1)
table(yhat.logic,caravan.test$Purchase)  # "No" is 0 and "Yes" is 1. 
(4183+58)/(4183+58+350+231) #Test Accuracy is 87.95%
# 58 out of (350+58=408) were predicted correctly to make the purchase i.e 58/408=0.142
#-----------------------------------------------






