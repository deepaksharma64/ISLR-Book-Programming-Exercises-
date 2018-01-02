#--------------CH4 KNN Practice--------------------------------------
library(ISLR)
library(MASS)
library(class)   #--------knn function in class library---------------
names(Smarket)   #-----1250x9-----------
str(Smarket)
summary(Smarket)
cor(Smarket)  #Error because there is Direction predictor which is notnumeric
cor(Smarket[,1:8])

attach(Smarket)
train=(Year<2005)
train.2005=(!train)
train.X=cbind(Lag1,Lag2)
train.X1=train.X[train,]
test.X=train.X[train.2005,]
train.Direction= Direction[train]
Direction.2005=Direction[!train]

set.seed(1)
knn.pred=knn(train.X1,test.X,train.Direction,k=1)   #--for k=1-------
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)   #----50%

knn.pred=knn(train.X1,test.X,train.Direction,k=3)   #--for k=3-------
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)   #-----53.5%

#--------KNN to Caravan Dataset-------------------------
dim(Caravan)      #---5822x86---------------------------
attach(Caravan)
summary(Purchase)
Standardized.X=scale(Caravan[,-86])
var(Caravan[,1])    #-----165.03
mean(Caravan[,1])   #-----24.253
var(Standardized.X[,1])   #----1
mean(Standardized.X[,1])  #----7.0255

Test.Caravan=Standardized.X[1:1000,]
Train.Caravan=Standardized.X[1001:5474,]
set.seed(1)
model.knn=knn(Train.Caravan,Test.Caravan,Caravan[1001:5474,86],k=1)
table(model.knn,Caravan[1:1000,86])
mean(model.knn==Caravan[1:1000,86]) #-----accuracy is 88% --this is of no interest based on details
mean_yes=9/(71+9)
mean_yes #------11.25% of people bought it who said yes.....
#---------k=5--------------
Test.Caravan=Standardized.X[1:1000,]
Train.Caravan=Standardized.X[1001:5474,]
set.seed(1)
model.knn5=knn(Train.Caravan,Test.Caravan,Caravan[1001:5474,86],k=5)
table(model.knn5,Caravan[1:1000,86])
mean(model.knn5==Caravan[1:1000,86]) #-----accuracy is 93% --this is of no interest based on details
mean_yes5=4/(4+11)
mean_yes5    #----accuracy increases to 27%-----------------


