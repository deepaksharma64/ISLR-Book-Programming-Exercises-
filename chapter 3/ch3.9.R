#--------(9)-----------------------------
library(ISLR)
library(MASS)
data(Auto)
names(Auto)
library(car)
#-------(a)-------------------
pairs(Auto)
#--------(b)------------------
dim(Auto)
# rows=392 Columns=9
numAuto=Auto[1:392,1:8]
names(numAuto)
cor(numAuto)
#---------(c)-----------------
lm.fitAuto=lm(mpg~.,data=numAuto)
summary(lm.fitAuto)
#(i)Yes there is a relation because R^2 is close to 1(~0.8) and P-value is very small.
#(ii)Weight looks most corelated and then Year
#(iii)Year coefficient is highest suggesting mpg improved significaty with years.
#---------(d)-----------------
par(mfrow=c(2,2))
plot(lm.fitAuto)
#Yes, Clear outliers are 334,323,330. 117 is a high leverage point.
#---------(e)------------------
lm.fit_1=lm(mpg~displacement+horsepower+weight+year+origin,data=numAuto)
summary(lm.fit_1)
lm.fit_2=lm(mpg~displacement+horsepower+year+origin*weight,data=numAuto)
summary(lm.fit_2)
lm.fit_3=lm(mpg~displacement+horsepower+weight+origin*year,data=numAuto)
summary(lm.fit_3)
lm.fit_4=lm(mpg~displacement+weight+year+origin*horsepower,data=numAuto)
summary(lm.fit_4)
lm.fit_5=lm(mpg~horsepower+weight+year+origin*displacement,data=numAuto)
summary(lm.fit_5)
lm.fit_6=lm(mpg~displacement+horsepower+year*weight+origin,data=numAuto)
summary(lm.fit_6)
lm.fit_7=lm(mpg~displacement+weight+year*horsepower+origin,data=numAuto)
summary(lm.fit_7)
lm.fit_8=lm(mpg~horsepower+weight+year*displacement+origin,data=numAuto)
summary(lm.fit_8)
lm.fit_9=lm(mpg~displacement+weight*horsepower+year+origin,data=numAuto)
summary(lm.fit_9)
lm.fit_10=lm(mpg~horsepower+weight*displacement+year+origin,data=numAuto)
summary(lm.fit_10)
lm.fit_11=lm(mpg~displacement*horsepower+weight+year+origin,data=numAuto)
summary(lm.fit_11)
#Yes,in almost all the cases there is increase in R^2 values with highest (0.89) in 9th one.
#------------------(f)-------------------
lm.fit_12=lm(mpg~displacement+horsepower+weight+year+I(origin^2),data=numAuto)
summary(lm.fit_12)
lm.fit_13=lm(mpg~displacement+horsepower+weight+year+I(log(origin)),data=numAuto)
summary(lm.fit_13)
lm.fit_14=lm(mpg~displacement+horsepower+weight+year+I(origin^0.5),data=numAuto)
summary(lm.fit_14)
lm.fit_15=lm(mpg~displacement+horsepower+weight+I(year^2)+origin,data=numAuto)
summary(lm.fit_15)
lm.fit_16=lm(mpg~displacement+horsepower+weight+I(year^0.5)+origin,data=numAuto)
summary(lm.fit_16)
lm.fit_17=lm(mpg~displacement+horsepower+weight+I(log(year))+origin,data=numAuto)
summary(lm.fit_17)
#--many possibilities---few of them I tried show little to no improvements-------
#--------problem 9 finished-----------------------------------