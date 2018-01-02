#-------------(11)-------
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
#---------(a)------------
par(mfrow=c(1,1))
plot(x,y)
lm.fit1=lm(y~x+0)
summary(lm.fit1)
abline(lm.fit1)  #-Coeff 1.99,std.er= 0.106,RSE=0.95,R^2=0.777,p-values close to zero.
#---------(b)------------
lm.fit2=lm(x~y+0)
summary(lm.fit2)
plot(y,x)
abline(lm.fit2) #-----Coff 0.39111,std.er=0.020,RSE=0.424,R^2=0.779,p-values close to zero
#----------(c)---------------
#both (a) and (b) look very similar.
#------------(f)-----------------
lm.fit3=lm(y~x)
summary(lm.fit3)
lm.fit4=lm(x~y)
summary(lm.fit4)
#it shows clearly that both have very similar t-values.it can be tested for more random variables


