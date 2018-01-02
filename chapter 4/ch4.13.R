#--------------------------13-----------------------------------
library(MASS)
library(ISLR)
library(class)
names(Boston)
dim(Boston)  #-----------506x14---------------------------------
attach(Boston)
str(Boston)  #----all columns are numeric or integer variables.
Med=median(crim)   #Med=0.25651
plot(Boston) #rm,age,dis,istat,medv looks related to crim.
#-----------------------------------------------------------------------------
Mat=rep(0,506)
Mat[Boston$crim>=Med]=1   #greater than median values are set to 1. 
BostonM=Boston
BostonM[,1]=Mat #crim values are replaced with 1 or 0s in modified data BostonM
#------------Logistic Regression---subset1------------------------------------
Train1d=c(1:456)
Test1d=c(457:506)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
lr=glm(crim ~.,data=Train1,family=binomial) #ptratio,rad,dis,nox,medv looks relevant
lr1=glm(crim ~ptratio+rad+dis+nox+medv,data=Train1,family=binomial)
fitlr1=predict(lr1,Test1,type="response")
fitlr1Q=fitlr1
fitlr1Q[fitlr1>=Med]=1
fitlr1Q[fitlr1<=Med]=0
table(fitlr1Q,TestMat)
mean(fitlr1Q==TestMat)   #0.7 
#------------Logistic Regression---subset2------------------------------------
Train1d=c(51:506)
Test1d=c(1:50)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
lr1=glm(crim ~ptratio+rad+dis+nox+medv,data=Train1,family=binomial)
fitlr1=predict(lr1,Test1,type="response")
fitlr1Q=fitlr1
fitlr1Q[fitlr1>=Med]=1
fitlr1Q[fitlr1<=Med]=0
table(fitlr1Q,TestMat)
mean(fitlr1Q==TestMat)   #0.46 
#------------Logistic Regression---subset3--------------------------------------
Train1d=c(1:99,151:506)
Test1d=c(100:150)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
lr1=glm(crim ~ptratio+rad+dis+nox+medv,data=Train1,family=binomial)
fitlr1=predict(lr1,Test1,type="response")
fitlr1Q=fitlr1
fitlr1Q[fitlr1>=Med]=1
fitlr1Q[fitlr1<=Med]=0
table(fitlr1Q,TestMat)
mean(fitlr1Q==TestMat)   #0.49
#---------------------------------------------------------------------------------
#----------------------LDA-------subset 1-----------------------------------------
Train1d=c(1:456)
Test1d=c(457:506)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
ld1=lda(crim ~ptratio+rad+dis+nox+medv,data=Train1)
fitld1=predict(ld1,Test1,type="response")
newP=fitld1$posterior[,2]
newP[fitld1$posterior[,2]>=Med]="1"  #adjusting 0's and 1's according to median
newP[fitld1$posterior[,2]<Med]="0"
table(newP,TestMat)
mean(newP==TestMat) # 0.7-----------------------------------------------------------
#----------------------LDA-------subset 2-----------------------------------------
Train1d=c(51:506)
Test1d=c(1:50)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
ld1=lda(crim ~ptratio+rad+dis+nox+medv,data=Train1)
fitld1=predict(ld1,Test1,type="response")
newP=fitld1$posterior[,2]
newP[fitld1$posterior[,2]>=Med]="1"  #adjusting 0's and 1's according to median
newP[fitld1$posterior[,2]<Med]="0"
table(newP,TestMat)
mean(newP==TestMat) # 0.56-----------------------------------------------------------
#----------------------LDA-------subset 3--------------------------------------------
Train1d=c(1:50,101:506)
Test1d=c(51:100)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
ld1=lda(crim ~ptratio+rad+dis+nox+medv,data=Train1)
fitld1=predict(ld1,Test1,type="response")
newP=fitld1$posterior[,2]
newP[fitld1$posterior[,2]>=Med]="1"  #adjusting 0's and 1's according to median
newP[fitld1$posterior[,2]<Med]="0"
table(newP,TestMat)
mean(newP==TestMat) # 1.0 (predicts everything as 0 on test set)
#----------------------LDA---------subset 4------------------------------------------
Train1d=c(1:215,267:506)
Test1d=c(216:266)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
ld1=lda(crim ~ptratio+rad+dis+nox+medv,data=Train1)
fitld1=predict(ld1,Test1,type="response")
newP=fitld1$posterior[,2]
newP[fitld1$posterior[,2]>=Med]="1"  #adjusting 0's and 1's according to median
newP[fitld1$posterior[,2]<Med]="0"
table(newP,TestMat)
mean(newP==TestMat) # 0.6 
#-------------------------------------------------------------------------------------
#----------------------KNN-------subset 1---------------------------------------------
Train1d=c(1:456)
Test1d=c(457:506)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
TrainMatrix=cbind(Train1$ptratio,Train1$rad,Train1$dis,Train1$nox,Train1$medv)
TestMatrix=cbind(Test1$ptratio,Test1$rad,Test1$dis,Test1$nox,Test1$medv)
kn1=knn(TrainMatrix,TestMatrix,Train1[,1],k=10) 
table(kn1,TestMat)
mean(kn1==TestMat) # 0.84-----------------------------------------------------------
#----------------------KNN-------subset 2---------------------------------------------
Train1d=c(51:506)
Test1d=c(1:50)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
TrainMatrix=cbind(Train1$ptratio,Train1$rad,Train1$dis,Train1$nox,Train1$medv)
TestMatrix=cbind(Test1$ptratio,Test1$rad,Test1$dis,Test1$nox,Test1$medv)
kn1=knn(TrainMatrix,TestMatrix,Train1[,1],k=10)
table(kn1,TestMat)
mean(kn1==TestMat) # 0.76-----------------------------------------------------------
#----------------------KNN-------subset 3---------------------------------------------
Train1d=c(1:100,151:506)
Test1d=c(101:50)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
TrainMatrix=cbind(Train1$ptratio,Train1$rad,Train1$dis,Train1$nox,Train1$medv)
TestMatrix=cbind(Test1$ptratio,Test1$rad,Test1$dis,Test1$nox,Test1$medv)
kn1=knn(TrainMatrix,TestMatrix,Train1[,1],k=10)
table(kn1,TestMat)
mean(kn1==TestMat) # 1.0 (predicts everything as 0 on test set)

#----------------------KNN-------subset 4---------------------------------------------
Train1d=c(1:215,267:506)
Test1d=c(216:266)
TestMat=Mat[Test1d]
Train1=BostonM[Train1d,]
Test1=BostonM[Test1d,]
TrainMatrix=cbind(Train1$ptratio,Train1$rad,Train1$dis,Train1$nox,Train1$medv)
TestMatrix=cbind(Test1$ptratio,Test1$rad,Test1$dis,Test1$nox,Test1$medv)
kn1=knn(TrainMatrix,TestMatrix,Train1[,1],k=10)
table(kn1,TestMat)
mean(kn1==TestMat) # 0.54   all knn susets can be tested for different k values(1,10,30,80 etc)
#--------------------------------------------------------------------------------------
#comment -- knn seems to have best accuracy among three tested models------------------

