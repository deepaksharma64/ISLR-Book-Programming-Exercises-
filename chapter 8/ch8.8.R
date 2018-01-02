#-------------------------ch-8--------(8)---------------------------------
library(ISLR)
library(tree)
library(dplyr)
library(ggplot2)
library(randomForest)
library(gbm)
library(reshape2)
#----------------------(a)------------
dim(Carseats)  #400x11
names(Carseats)
attach(Carseats)
set.seed(2)
train=sample(1:nrow(Carseats),(nrow(Carseats)/2))
Carseats.test=Carseats[-train,]
Sales.test=Sales[-train]
#----------------------(b)-------------------
tree.carseats=tree(Sales~.,Carseats,subset = train)
tree.pred=predict(tree.carseats,Carseats.test)
err=mean((tree.pred-Sales.test)^2)  #Test MSE=4.844
plot(tree.carseats)
text(tree.carseats,pretty=0)
summary(tree.carseats)  #no. of terminal nodes are 17 and only 
                        #seven variables are used to create the tree.
#----------------------(c)--pruning------------------
cv.carseats=cv.tree(tree.carseats)
plot(cv.carseats$size,cv.carseats$dev,type="b")
cv.carseats #tree=15 is has lowest cv error which is very close to unpruned tree=17.
prune.carseats=prune.tree(tree.carseats,best=15)
tree.pred=predict(prune.carseats,Carseats.test)
err=mean((tree.pred-Sales.test)^2) #Test MSE=4.88
plot(prune.carseats)
text(prune.carseats)
#Pruning does not improve the test error
#-----------------------(d)---------------------------
#Bagging---
set.seed(1)
bag.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=10,importance=TRUE)
bag.carseats #default no. of trees=500
yhat.bag=predict(bag.carseats,newdata=Carseats[-train,])
mean((yhat.bag-Sales.test)^2) #2.42 for mtry=10
importance(bag.carseats) #Priceb and ShelveLoc are important
varImpPlot(bag.carseats)
#-----------------------(e)----------------------------
#Random Forest---
set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,mtry=7,importance=TRUE)
rf.carseats
yhat.rf=predict(rf.carseats,newdata=Carseats[-train,])
mean((yhat.rf-Sales.test)^2) #2.9 for mtry=3 #2.5 for mtry=5 #2.47 for mtry=7
importance(bag.carseats)   #Priceb and ShelveLoc are important
varImpPlot(bag.carseats)
#m=10 i.e bagging is performing best on the test set used here.
#-------------------------------------------------------


