#--------------------14--------------------------
#----(a)----
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
#beta0=2,beta1=2,beta2=0.3
#-----(b)------
cor(x1,x2)
#x1 and x2 are highly correlated =0.84
plot(x1,x2)
#--------(c)------------
lm.fit=lm(y~x1+x2)
summary(lm.fit)
#beta0=2.130,beta1=1.440,beta2=1.01
#beta0 pr(>|t|)=6e-15 hence can be rejected,cannot reject beta2 since pr(>|t|)0.375
#--------(d)----------------
lm.fitx1=lm(y~x1)
summary(lm.fitx1)
#Pr(>|t|) values are very close to zero hence we can reject null hypothesis.
#--------(e)-----------
lm.fitx2=lm(y~x2)
summary(lm.fitx2)
#Pr(>|t|) values are very close to zero hence we can reject null hypothesis.
#-------(f)------------
#No since x1 and x2 are highly correlated one of them becomes redudendent when considered together.
#--------(g)-----------
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)
lm.fit=lm(y~x1+x2)
summary(lm.fit) #RSE=1.1,R^2=0.219,Pr(>|t|) for beta0 = zero.Coeff 2.227,0.539,2.515
par(mfrow=c(2,2))#there is leverage: from residual vs lenerage plot
plot(lm.fit)
plot(hatvalues(lm.fit))
which.max (hatvalues (lm.fit))
lm.fitx1=lm(y~x1)
summary(lm.fitx1) #RSE=1.1,R^2=0.156,Both Pr(>|t|) close to zero.Coeff 2.257,1.766
par(mfrow=c(2,2)) #there is outlier : from residual vs fitted and q-q plots
plot(lm.fitx1)
plot(hatvalues(lm.fitx1))
which.max (hatvalues (lm.fitx1))
lm.fitx2=lm(y~x2)
summary(lm.fitx2) #RSE=1.1,R^2=0.212,Both Pr(>|t|) close to zero.Coeff 2.34,3.11
par(mfrow=c(2,2)) #there is a leverage from residual vs leverage plot
plot(lm.fitx2)
plot(hatvalues (lm.fitx2)) 
which.max (hatvalues (lm.fitx2))

