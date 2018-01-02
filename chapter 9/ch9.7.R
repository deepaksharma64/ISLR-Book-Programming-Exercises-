#--------------------ch-9----(7)------------
library(ISLR)
library(e1071)
library(dplyr)
library(class)
#------------------------(a)----------------
dim(Auto) #392x9
names(Auto)  #[mpg,cylinders,displacement,horsepower,weight,acceleration,year,origin,name]
mpgF=1*(Auto$mpg>median(Auto$mpg))
Auto= mutate(Auto,mpgF=as.factor(mpgF)) #dplyr package
names(Auto)
Auto=dplyr::select(Auto,-mpg) #dplyr package 
names(Auto) #mpg column is removed since mpgF was added 
#------------------------(b)----------------
set.seed(1)
tune.out=tune(svm,mpgF~.,data=Auto,kernel="linear",range=list(cost=c(0.001,0.01,0.1,1,5,10,100)))  
summary(tune.out)
#CVerrors are [0.132,0.091,0.096,0.091,0.101,0.117]
#minimum CVerror for cost=0.01 and 1. cost=1 gives best performance.
#------------------------(c)-----------------
set.seed(1) #Radial--------------------------
tune.out=tune(svm,mpgF~.,data=Auto,kernel="radial",range=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=c(0.5,1,2,3,4)))  
summary(tune.out) #best performance cost=1,gamma=1
tune.out=tune(svm,mpgF~.,data=Auto,kernel="radial",range=list(cost=c(0.001,0.01,0.1,1,5,10,100),gamma=1))  
summary(tune.out) #CVerrors (gamma=1) are [0.5587,0.5587,0.5587,0.0816,0.0866,0.0841,0.0866]

set.seed(1)  #Polynomial Kernel------------
tune.out=tune(svm,mpgF~.,data=Auto,kernel="polynomial",range=list(cost=c(0.001,0.01,0.1,1,5,10,100),degree=c(0.5,1,2,3,4)))  
summary(tune.out) #best performance cost=100,degree=1
tune.out=tune(svm,mpgF~.,data=Auto,kernel="polynomial",range=list(cost=c(0.001,0.01,0.1,1,5,10,100),degree=1))  
summary(tune.out) #CVerrors (degree=1) are [0.558,0.558,0.248,0.107,0.089,0.094,0.102]
#Minimum CV errors for 3 models: Radial=0.0816,Ploynomial=0.089,linear=0.091 
#--------------------------(d)----------------------------------------------
pairs(Auto) 

#linear
svmfit.linear=svm(mpgF~.,data=Auto,kernel="linear",cost=1)
plot(svmfit.linear,Auto,horsepower~acceleration)
plot(svmfit.linear,Auto,weight~acceleration)

#radial
svmfit.radial=svm(mpgF~.,data=Auto,kernel="radial",gamma=1,cost=1)
plot(svmfit.radial,Auto,horsepower~acceleration)
plot(svmfit.radial,Auto,weight~acceleration)

#polynomial
svmfit.polynomial=svm(mpgF~.,data=Auto,kernel="polynomial",degree=1,cost=100)
plot(svmfit.polynomial,Auto,horsepower~acceleration,slice=sa)
plot(svmfit.polynomial,Auto,weight~acceleration,slice=sb)
#Note: these plot functions are not giving proper color codes for 1 and 0.
#----------------------------------------------------------------------------

