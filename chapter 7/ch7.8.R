#----------------------------------ch7 (8)--------------------------------------------------
library(ISLR)
library(splines)
library(gam)
library(MASS)
library(boot)

dim(Auto) #392x9
names(Auto) #"mpg","cylinders","displacement","horsepower","weight","acceleration" "year","origin","name"
#Auto$name is a factor with names.
Auto=Auto[,1:8]
dim(Auto) #392x8

fit1=lm(mpg~poly(displacement,1),data=Auto)
fit2=lm(mpg~poly(displacement,2),data=Auto)
fit3=lm(mpg~poly(displacement,3),data=Auto)
fit4=lm(mpg~poly(displacement,4),data=Auto)
fit5=lm(mpg~poly(displacement,5),data=Auto)
anova(fit1,fit2,fit3,fit4,fit5)   #polynomial 2 for displacemet

fit1=lm(mpg~poly(horsepower,1),data=Auto)
fit2=lm(mpg~poly(horsepower,2),data=Auto)
fit3=lm(mpg~poly(horsepower,3),data=Auto)
fit4=lm(mpg~poly(horsepower,4),data=Auto)
fit5=lm(mpg~poly(horsepower,5),data=Auto)
fit6=lm(mpg~poly(horsepower,6),data=Auto)
fit7=lm(mpg~poly(horsepower,7),data=Auto)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7) # polynomial 5 for horsepower

fit1=lm(mpg~poly(weight,1),data=Auto)
fit2=lm(mpg~poly(weight,2),data=Auto)
fit3=lm(mpg~poly(weight,3),data=Auto)
fit4=lm(mpg~poly(weight,4),data=Auto)
fit5=lm(mpg~poly(weight,5),data=Auto)
fit6=lm(mpg~poly(weight,6),data=Auto)
fit7=lm(mpg~poly(weight,7),data=Auto)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7) # polynomial 2 for weight

fit1=lm(mpg~poly(weight,1),data=Auto)
fit2=lm(mpg~poly(weight,2),data=Auto)
fit3=lm(mpg~poly(weight,3),data=Auto)
fit4=lm(mpg~poly(weight,4),data=Auto)
fit5=lm(mpg~poly(weight,5),data=Auto)
fit6=lm(mpg~poly(weight,6),data=Auto)
fit7=lm(mpg~poly(weight,7),data=Auto)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7) # polynomial 2 for weight

fit1=lm(mpg~poly(acceleration,1),data=Auto)
fit2=lm(mpg~poly(acceleration,2),data=Auto)
fit3=lm(mpg~poly(acceleration,3),data=Auto)
fit4=lm(mpg~poly(acceleration,4),data=Auto)
fit5=lm(mpg~poly(acceleration,5),data=Auto)
fit6=lm(mpg~poly(acceleration,6),data=Auto)
fit7=lm(mpg~poly(acceleration,7),data=Auto)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7) # polynomial 4 for acceleration
set.seed(1)
finalfitP=gam(mpg~poly(displacement,2)+poly(horsepower,5)+poly(weight,2)+poly(acceleration,4)+cylinders+origin,data=Auto)
err=cv.glm(Auto,finalfitP,K=10)$delta[1] #err=15.34
set.seed(2)
#lets test err for linear fit
finalfit=gam(mpg~displacement+horsepower+weight+acceleration+cylinders+origin,data=Auto)
err=cv.glm(Auto,finalfit,K=10)$delta[1] #err=17.99
#Clearly err values suggest that polynomial fit is the right way to go.
pairs(Auto) #plots suggest that predictors do not have linear relationship with mpg
plot(finalfitP,se=TRUE)
plot(finalfit,se=TRUE)
#------------------------------------------------------------------------------------------------