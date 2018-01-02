#--------------------------ch8-----(12)------------
library(ISLR)
library(tree)
library(dplyr)
library(ggplot2)
library(randomForest)
library(gbm)
dim(Wage)  #3000x12  We will use Wage data set in ISLR Package
names(Wage) #year,age,sex,maritl,race,education,region,jobclass,health,health_ins,logwage,wage.
str(Wage)
Wage=Wage[,1:11] #removing wage column since logwage is already a column.
dim(Wage)
names(Wage)
set.seed(1)
train=sample(nrow(Wage),2200)
train.wage=Wage[train,]
test.wage=Wage[-train,]

#Bagging---
set.seed(2)
bag.wage=randomForest(logwage~.,data=train.wage,mtry=10,importance=TRUE)
yhat.bag=predict(bag.wage,newdata=test.wage)
plot(yhat.bag,test.wage$logwage)
abline(0,1)
mean((yhat.bag-test.wage$logwage)^2) #0.0806
#Random Forest---
set.seed(3)
rf.wage=randomForest(logwage~.,data=train.wage,mtry=3,importance=TRUE)
yhat.rf=predict(rf.wage,newdata=test.wage)
plot(yhat.rf,test.wage$logwage)
abline(0,1)
mean((yhat.rf-test.wage$logwage)^2) #0.0708
importance(rf.wage) #Education,age,health_ins and maritl are most important predictors.
varImpPlot(rf.wage)
#-------------------------------Boosting----------------------------------------------------
set.seed(4)
boost.wage=gbm(logwage~.,data=train.wage,distribution = "gaussian",n.trees = 5000,interaction.depth=1)
boost.wage
summary(boost.wage)
yhat.boost=predict(boost.wage,newdata=test.wage,n.trees=5000)
mean((yhat.boost-test.wage$logwage)^2) #0.0721
set.seed(5)
boost.wage=gbm(logwage~.,data=train.wage,distribution = "gaussian",n.trees = 5000,interaction.depth=4,shrinkage=0.2,verbose = F)
yhat.boost=predict(boost.wage,newdata=test.wage,n.trees=5000)
mean((yhat.boost-test.wage$logwage)^2) #0.1132
#minimum test error was given by randomforest .0708 followed by boosting 0.0721 and then bagging 0.0806
#--------------------------------Linear Regression-----------------------------------------------------------
linear.wage=glm(logwage~poly(year,3)+poly(age,3)+education+health+health_ins+jobclass+race+maritl,data=train.wage)
yhat.linear=predict(linear.wage,newdata=test.wage)
mean((yhat.linear-test.wage$logwage)^2) #0.069
#linear regression has the lowest test error of 0.069 for this data.
#-------------------------------------------------------------------------------------------------
