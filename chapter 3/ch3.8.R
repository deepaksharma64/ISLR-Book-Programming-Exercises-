#EXCERCISE 8$ -------------------------------------------------------------------
library(ISLR)
library(MASS)
data(Auto)
names(Auto)
lm.fit=lm(mpg~horsepower,data=Auto)
summary(lm.fit)
#---------------(a)------------------------------#
#Since p-value is very low, there is a relation between mpg and horsepower#
#R2 value is 0.60 which indicates there is correlation between two of them#
#Relationship is negative since coffecients have opposite signs#
value=data.frame(horsepower=98)
predict(lm.fit,value)
predict(lm.fit,value,interval="prediction")
predict(lm.fit,value,interval="confidence")
#---------------(b)-----------------------------#
plot(Auto$horsepower,Auto$mpg)
abline(lm.fit,col='red')
#---------------(c)-----------------------------#
par(mfrow=c(2,2))
plot(lm.fit)
#There is a problem with the fit since there is a strong U-shape pattern of residuas
#suggesting linear fit is not accurate.

