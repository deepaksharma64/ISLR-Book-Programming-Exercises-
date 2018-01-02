#-------------------------ch-8----(9)-
library(ISLR)
library(tree)
library(dplyr)
library(ggplot2)
library(randomForest)
#----------------------(a)--------------------------
dim(OJ) #1070x18
names(OJ)
set.seed(10)
train=sample(nrow(OJ),800)
oj.test=OJ[-train,]
oj.purchase.test=OJ[-train,"Purchase"]
oj.train=OJ[train,]
attach(OJ)
#----------------------(b)--------------------------
tree.oj=tree(Purchase~.,OJ,subset = train)
summary(tree.oj)  
#Number of terminal nodes:7
#Misclassification error rate: 0.1625 = 130 / 800
#--------------------(c)---------------------------------
tree.oj  #We pick second one ###2) LoyalCH < 0.450956 290  295.700 MM ( 0.20690 0.79310 )
#split is based on : LoyalCH < 0.450956 290
#n=290 observations are there in the branch
#deviance of the node is 295.77
#prediction for this terminal node is always "MM".
#Probability of "CH" is 0.2069 and Probability of "MM" is 0.7931 for this node terminal.
#---------------------(d)--------------------------------
plot(tree.oj)
text(tree.oj,pretty=0) #Three of the nodes lead to MM prediction and other 4 predict CH. Tree is not complicated and easyn to undestand.
#---------------------(e)------------------------------
tree.pred=predict(tree.oj,oj.test,type="class") #--checking the test error
table(tree.pred,oj.purchase.test)
(155+66)/(155+66+22+27)  #81.85%  accuracy.
#----------------------(f)--------------------
set.seed(5)
cv.oj=cv.tree(tree.oj,FUN=prune.misclass)
cv.oj #tree=5 is has low cv error and hence it is optimal tree size in this scenario.
#----------------------(g)--------------------
plot(cv.oj$size,cv.oj$dev,type="b")
#----------------------(h)--------------------
#tree=5 is has low cv error
#----------------------(i)--------------------
prune.oj=prune.tree(tree.oj,best=5)
tree.pred=predict(prune.oj,oj.test,type="class")
plot(prune.oj)
text(prune.oj)
summary(prune.oj) #Misclassification rate 0.178=143/800
#----------------------(j)---------------------
#Misclassification training error rate of unpruned tree: 130/800=0.162
#Misclassification training error rate of pruned tree: 143/800=0.178
#----------------------(k)---------------------
table(tree.pred,oj.purchase.test)
(136+81)/(136+81+41+12)   #80.37 
#test error before pruning = (1-0.818)=0.182
#test error after prunung = (1-0.803)=0.197
#----------------------------------------------
