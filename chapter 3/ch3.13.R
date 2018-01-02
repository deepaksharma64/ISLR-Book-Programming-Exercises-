#-----------------------13-----------
#---------------(a)------------------
set.seed(1)
x=rnorm(100,mean=0,sd=1) 
#----------------------(b)------------
eps=rnorm(100,mean=0,sd=0.5)
#----------------------(c)------------
y=-1+0.5*x+eps
Ly=length(y)
#Length of y is 100 and beta0=-1,beta1=0.5
#---------------(d)-----------------------
plot(x,y)
#----x and y looks positively correlated----
#---------------(e)----------------------
lm.fit=lm(y~x)
summary(lm.fit)
#beta0=-1.0188,beta1=0.4995 which is close to the linear model betas.
#----------------(f)--------------------
abline(a=-1,b=0.5,col="red",lwd=3)
abline(lm.fit,col="blue",lwd=3)
legend(x = c(-2,-1), y = c(-0.5,0.0), legend = c("population", "model fit"), col = c("blue","red"), lwd=5 )
#-----------------(g)-------------------
lm.fit1=lm(y~x+I(x^2))
summary(lm.fit1)
anova(lm.fit,lm.fit1)
#R2=0.467 for x and R2=0.478 when x^2 abd x (since R^2 is low for both change is irrelevant).Pr for t statistic of x^2 is 0.16 means no improvement
#-----------------(h)-------------------------------
set.seed(1)
x=rnorm(100,mean=0,sd=1) 
eps=rnorm(100,mean=0,sd=0.05) #cganged it to 0.05 from 0.5
y=-1+0.5*x+eps
Ly=length(y)
plot(x,y)
lm.fit2=lm(y~x)
summary(lm.fit2)
abline(a=-1,b=0.5,col="red",lwd=3)
abline(lm.fit2,col="blue",lwd=3)
legend(x = c(-2,-1), y = c(-0.5,0.0), legend = c("population", "model fit"), col = c("blue","red"), lwd=5 )
lm.fit3=lm(y~x+I(x^2))
summary(lm.fit3)
anova(lm.fit2,lm.fit3)
#with x model: R^2 is 0.989 both T and F statistic P are very low. RSE is only 0.048
#with x^2 model:R^2 is 0.989 both T and F statistic P are very low. RSE is only 0.048 but x^2 term is not useful as Pr for t-statistic is 0.16
#-----------------(i)-------------------------------
set.seed(1)
x=rnorm(100,mean=0,sd=1) 
eps=rnorm(100,mean=0,sd=5) #changed it to 5 from 0.5
y=-1+0.5*x+eps
Ly=length(y)
plot(x,y)
lm.fit4=lm(y~x)
summary(lm.fit4)
abline(a=-1,b=0.5,col="red",lwd=3)
abline(lm.fit4,col="blue",lwd=3)
legend(x = c(-2,-1), y = c(-0.5,0.0), legend = c("population", "model fit"), col = c("blue","red"), lwd=5 )
lm.fit5=lm(y~x+I(x^2))
summary(lm.fit5)
anova(lm.fit,lm.fit5)
#with x model: R^2 is 0.00853 both T and F statistic P are very high. RSE is only 4.8
#with x^2 model:R^2 is 0.0283 both T and F statistic P are very high. RSE is only 0.048 
#--------------(j)---------------------------
confint(lm.fit)
confint(lm.fit2)
confint(lm.fit4)
#confidence interval kept on widening for higher variations(higher sd value).
