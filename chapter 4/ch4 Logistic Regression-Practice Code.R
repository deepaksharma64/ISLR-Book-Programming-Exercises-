#--------------CH4--------------------------------------
library(ISLR)
library(MASS)
names(Smarket)
str(Smarket)
summary(Smarket)
cor(Smarket)
cor(Smarket[,1:8])
attach(Smarket)
plot(Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)
coef(glm.fits)
B=summary(glm.fits)
B$coef[,4]
Prob=predict(glm.fits,type="response") #Are there Prob for Up or Down ?
Prob[1:10]
contrasts(Direction) #Up is asspociated with 1 and that is the default.
glm.rep=rep("Down",1250)
glm.rep[Prob>0.5]="Up"
table(glm.rep,Direction)
(507+145)/1250
mean(glm.rep == Direction) #on training set accuracy is about 52%--------------

train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

fit.training=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
test2005=predict(fit.training,Smarket.2005,type="response")
rep2005=rep("Down",252)
rep2005[test2005>0.5]="Up"
table(rep2005,Direction.2005)
mean(rep2005==Direction.2005)  #only 48% accuracy which is less than the random guess
mean(rep2005!=Direction.2005)

#Only using Lag1 and Lag2 as predictors as it had lowest p values
fit.training=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
test2005=predict(fit.training,Smarket.2005,type="response")
rep2005=rep("Down",252)
rep2005[test2005>0.5]="Up"
table(rep2005,Direction.2005)
mean(rep2005==Direction.2005) #here 56% accuracy---------------
mean(rep2005!=Direction.2005)

#---------predicting for some values of Lag1 and Lag2----------
newdata=predict(fit.training,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")
newdata
