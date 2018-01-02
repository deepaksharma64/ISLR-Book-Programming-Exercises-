#--------------CH4 QDA Practice--------------------------------------
library(ISLR)
library(MASS)
names(Smarket)
str(Smarket)
summary(Smarket)
cor(Smarket)  #Error because there is Direction predictor whic is notnumeric
cor(Smarket[,1:8])
attach(Smarket)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

fit.training=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
fit.training
par(mfrow=c(2,2))
plot(fit.training)

test2005=predict(fit.training,Smarket.2005,type="response")
drep2005=test2005$class
table(drep2005,Direction.2005)
mean(drep2005==Direction.2005) #here 56% accuracy---------------
mean(drep2005!=Direction.2005)
#-----------threshold can be changed from 50% for up and down-------------
up70=(test2005$posterior[,2]>0.7)
totalup70=sum(up70)
totalup70
#---------predicting for some values of Lag1 and Lag2---------------------
newdata=predict(fit.training,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")
newdata             
#-------------------------------------------------------------------------