#----------(10)-------------------
library(ISLR)
library(MASS)
library(car)
data(Carseats)
names(Carseats)
#--------(a)-------------------
lm.predict=lm(Sales~Price+Urban+US,data=Carseats)
summary(lm.predict)
#--------(b)------------------
#Urban and US-inversly proportional while Price is directly prop to Sales data.
dim(Carseats) #-------------400 11------------------#
summary(Carseats)
#-----10:Urban and 11:US are non numeric-----------
Carseats1=Carseats[,1:9]
lm.predict1=lm(Carseats1)
summary(lm.predict1)
#------(c)------------------
#Equation: Sales= 13.043-0.544*Price-0.0219*UrbanYes+1.20*USYes
#------(d)------------------
#We can reject null-Hypothesis for Price and USYes since P(t) is very close to zero......
#------(e)-----------------
names(Carseats)
lm.predict2=lm(Sales~Price+US,data=Carseats)
summary(lm.predict2)
#-------(f)---------
#R^2 for (a) is 0.233 while for (d) is 0.235, a slight improvement.
#-------(g)---------
confint(lm.predict2)
#-------(h)---------
par(mfrow=c(2,2))
plot(lm.predict2)
#Yes there are outliers and high leverage values as well.
plot(hatvalues (lm.predict2))
which.max (hatvalues (lm.predict2))
#------------done-------------------#