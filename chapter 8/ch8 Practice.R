#------------------------------ch-8---Practice------------------------------------
library(ISLR)
library(tree)
library(dplyr)
library(ggplot2)
library(randomForest)
library(gbm)
dim(Carseats)  #400x11
names(Carseats)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
dim(Carseats) #400x12
tree.Carseats
tree.Carseats=tree(High~.-Sales,Carseats)
summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats,pretty=0)
#--checking the test error----------------------------------------------------
set.seed(2)
train=sample(1:nrow(Carseats),(nrow(Carseats)/2))
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset = train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/(86+57+27+30)  #71.5%  Overfitted the data.
#------pruning----------------------------------------------------------------
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) #$size=4 
cv.carseats

prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/(24+22+94+60) #77%  Pruning improves it.
#------------------------------Fitting Regression Trees------------------------------------------------

set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)   #506x14
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")
cv.boston #most complex tree=9 is has lowest cv error
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston)
#use unpruned tree to make prediction
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(boston.test,yhat)
abline(0,1)
err=mean((yhat-boston.test)^2)  #24.487
#------------------------------------Bagging and Random Forest----------------------------------------------
#Bagging---
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) #16.730 
#changing number of trees
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE,ntree=25)
bag.boston
yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) #16.885
#Random Forest---
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
rf.boston
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
plot(yhat.rf,boston.test)
abline(0,1)
mean((yhat.rf-boston.test)^2) #17.821
importance(rf.boston)
varImpPlot(rf.boston)
#-------------------------------Boosting----------------------------------------------------
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees = 5000,interaction.depth=4)
boost.boston
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
plot(Boston)

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
plot(yhat.boost,boston.test)
abline(0,1)
mean((yhat.boost-boston.test)^2) #17.466
#-------------------------------------------------------------------------------------------
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees = 5000,interaction.depth=4,shrinkage=0.2,verbose = F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2) #15.45
#m=7 has the minimum test error
#-------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------