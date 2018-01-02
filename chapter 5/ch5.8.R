#------------------------8----------------------------------------
library(ISLR)
library(boot)
#-----------------------(a)----------------------------------------
set.seed(1)
x=rnorm(100)
y =x-2*x^2+ rnorm (100)   #n=100,p=2 (x and y)
#-----------------------(b)----------------------------------------
plot(x,y,col="red") #Relationship is not linear, its quadratic as suggested by equation.
#-----------------------(c)-----------------------------------------
Mat=data.frame(cbind(x,y))
names(Mat)=c("x","y")
set.seed(2)
eq1=glm(y~x,data=Mat)
eq2=glm(y~poly(x,2),data=Mat)
eq3=glm(y~poly(x,3),data=Mat)
eq4=glm(y~poly(x,4),data=Mat)
err1=cv.glm(Mat,eq1)
err1$delta[1]   #7.288
err2=cv.glm(Mat,eq2)
err2$delta[1] #0.9374  ---minimum error
err3=cv.glm(Mat,eq3)
err3$delta[1] #0.9566
err4=cv.glm(Mat,eq4)
err4$delta[1] #0.9539
#------------------------------(d)-------------------------------------  
set.seed(4)
eq1=glm(y~x,data=Mat)
eq2=glm(y~poly(x,2),data=Mat)
eq3=glm(y~poly(x,3),data=Mat)
eq4=glm(y~poly(x,4),data=Mat)
err1=cv.glm(Mat,eq1)
err1$delta[1]   
err2=cv.glm(Mat,eq2)
err2$delta[1]   
err3=cv.glm(Mat,eq3)
err3$delta[1] 
err4=cv.glm(Mat,eq4)
err4$delta[1] 
#results are not same because error term associated with y is randomly genrated everytime.
#-------------------------------(e)--------------------------------------------
#err2 is minimum,expected because data itself is quadratic in nature as shown by equation.
#--------------------------------(f)-------------------------------------------
summary(eq1) #Coeff_errors 0.261 0.290
summary(eq2) #Coeff_errors 0.0958 0.958,0.9580
summary(eq3) #Coeff_errors 0.0962,0.962,0.962.0.962
summary(eq4) #Coeff_errors 0.0959,0.959,0.959,0.959
#Again the minimum coefficients error is of quadratic fit as expected from crossvalidation results.
#------------------------------------------------------------------------------

