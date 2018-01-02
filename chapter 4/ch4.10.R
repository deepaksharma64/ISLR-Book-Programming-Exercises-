#-----------------(10)-------------------------------------------
library(class)
library(MASS)
library(ISLR)
names(Weekly)
dim(Weekly) #----1089x9---Weekly return for 21 years from 1990-2010

#-------------(a)-----------------------------------------------------
summary(Weekly)
pairs(Weekly)
#--from plotted graphs------------------------------------------------
#Volume and Year are directly proportional to each other.
#Other patterns are not celar from the graphs.

#------------(b)------------------------------------------------------
logistic=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
summary(logistic)
#Lag2 +ve correlation,Pr(>|z|)=.0296 and #Lag1 -ve correlation,Pr(>|z|)=.1181

#----------- (c)------------------------------------------------------
pred=predict(logistic,type="response")
ptest=rep("Down",1089)
ptest[pred>0.5]="Up"
attach(Weekly)
table(ptest,Direction)
mean(ptest==Direction)  #56% accuracy....
#54/(54+48)=53% correct when says down,and 557/(430+557)=56% correct when said Up.
#------------(d)-------------------------------------------------------
yeartr=(Year<2009)
Weekly.train=Weekly[yeartr,]
Weekly.test=Weekly[!yeartr,]
Direction.test=Direction[!yeartr]
Direction.train=Direction[yeartr]

fit.glm=glm(Direction~Lag2,data=Weekly,family=binomial,subset=yeartr) #--LogisticRegression--
predict.glm=predict(fit.glm,Weekly.test,type="response")
conflr=rep("Down",104)
conflr[predict.glm>0.5]="Up"
table(conflr,Direction.test) #(9+56)/104=62.5% Accuracy with logistic regression
mean(conflr==Direction.test)

#------------(e)-------------------------------------------------------------
fit.lda=lda(Direction~Lag2,data=Weekly,subset=yeartr) #--LDA--
predict.lda=predict(fit.lda,Weekly.test,type="response")
conflda=predict.lda$class
table(conflda,Direction.test) #(9+56)/104=62.5% Accuracy with LDA
mean(conflda==Direction.test)

#------------(f)-------------------------------------------------------------
fit.qda=qda(Direction~Lag2,data=Weekly,subset=yeartr) #--QDA--
predict.qda=predict(fit.qda,Weekly.test,type="response")
confqda=predict.qda$class
table(confqda,Direction.test) #(0+61)/104=58.65% Accuracy with QDA 
mean(confqda==Direction.test) #Down prediction is 0 always predicts Up

#------------(g)-------------------------------------------------------------
Tmatrix=matrix(Lag2)
train.X=matrix(Tmatrix[yeartr])
test.X=matrix(Tmatrix[!yeartr])
set.seed(1)
fit.knn=knn(train.X,test.X,Direction.train,k=1)
table(fit.knn,Direction.test) 
mean(fit.knn==Direction.test) #accuracy is 50% with knn.......

#-------------(h)------------------------------------------------------------
#lda and logistic regression both have 62.5% accuracy which is highest.

#-------------(i)------------------------------------------------------------
#--Logistic Regression---
yeartr=(Year<2009)
Weekly.train=Weekly[yeartr,]
Weekly.test=Weekly[!yeartr,]
Direction.test=Direction[!yeartr]
Direction.train=Direction[yeartr]

fit.glm=glm(Direction~Lag2+Lag1,data=Weekly,family=binomial,subset=yeartr) #--LogisticRegression--
predict.glm=predict(fit.glm,Weekly.test,type="response")
conflr=rep("Down",104)
conflr[predict.glm>0.5]="Up"
table(conflr,Direction.test) #58% Accuracy with logistic regression
mean(conflr==Direction.test)
#------------LDA-------------------------------------------------------------
fit.lda=lda(Direction~Lag2+Lag1,data=Weekly,subset=yeartr) #--LDA--
predict.lda=predict(fit.lda,Weekly.test,type="response")
conflda=predict.lda$class
table(conflda,Direction.test) #58% Accuracy with LDA
mean(conflda==Direction.test)
#------------QDA-------------------------------------------------------------
fit.qda=qda(Direction~Lag2+Lag1,data=Weekly,subset=yeartr) #--QDA--
predict.qda=predict(fit.qda,Weekly.test,type="response")
confqda=predict.qda$class
table(confqda,Direction.test) #55% accuracy with QDA 
mean(confqda==Direction.test) 
#------------KNN-------------------------------------------------------------
Tmatrix=cbind(Lag1,Lag2)
train.X=Tmatrix[yeartr,]
test.X=Tmatrix[!yeartr,]
set.seed(1)
fit.knn=knn(train.X,test.X,Direction.train,k=3)
table(fit.knn,Direction.test)
mean(fit.knn==Direction.test) #accuracy is 52%
#----------------------------------------------------------------------------
