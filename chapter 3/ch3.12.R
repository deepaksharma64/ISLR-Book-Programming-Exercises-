#----------------------(12)-------------------------------
#-----(a)---Equating both the equation shows when:
#Summation(xi^2)=Summation(yi^2) ==> xi=+-yi ==> beta=1 condition is met.
#-------(b)-------------------------
set.seed(1)
x=rnorm(100)
y=5*x+rnorm(100)
plot(x,y)
lm.fit1=lm(y~x)
abline(lm.fit1)
summary(lm.fit1)
lm.fit2=lm(x~y)
abline(lm.fit2)
summary(lm.fit2)
#x intercept estimate is -0.03 and 0.01 which are not equal to each other.
#------(c)--------------
set.seed(1)
x=rnorm(100,mean=10,sd=.001)
y=rnorm(100,mean=10,sd=.001)
options(digits=2)
plot(x,y)
lm.fit3=lm(y~x)
abline(lm.fit1)
summary(lm.fit1)
lm.fit4=lm(x~y)
abline(lm.fit2)
summary(lm.fit2)
#x intercept estimate is 10 and is same in both the cases.


