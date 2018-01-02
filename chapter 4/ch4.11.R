#-------------(11)------------------------------------------
library(MASS)
library(ISLR)
library(class)
knnsummary(Auto)
names(Auto)
dim(Auto)      #---392x9----
attach(Auto)
#--------------------(a)-------------------------------------
mpgmed=median(mpg)
mpg01=rep(5,392)  #matrix of 392x1 with 5 (just a dummy number to be replaced)as its element
mpg01[mpg>mpgmed]=1
mpg01[mpg<mpgmed]=0  #--Alternate: mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
AutoE=data.frame(Auto,mpg01)  #----392x10------
#------------------- (b)--------------------------------------
par(mfrow=c(1,1))
pairs(AutoE) #horespower,weight,acceleration looks like associated with mpg01
boxplot(mpg01)
boxplot(weight)
boxplot(acceleration) #acceleration looks very closely related
boxplot(horsepower)
#--------------------(c)-----------------------------------------
TrainAutoE=AutoE[1:342,]
TestAutoE=AutoE[343:392,]
#--------------------(d)-----------------------------------------
LDAutoE=lda(mpg01~horsepower+weight+acceleration,data=TrainAutoE)  #----LDA-----
LDApred=predict(LDAutoE,TestAutoE)
table(LDApred$class,TestAutoE$mpg01) #-----(38+3)/50=82% Accuracy, 18%error
#--------------------(e)------------------------------------------
QDAutoE=qda(mpg01~horsepower+weight+acceleration,data=TrainAutoE)  #----QDA-----
QDApred=predict(QDAutoE,TestAutoE)
table(QDApred$class,TestAutoE$mpg01) #-----(39+4)/50=86% Accuracy, 14%error
#--------------------(f)-------------------------------------------
LOGAutoE=glm(mpg01~horsepower+weight+acceleration,family=binomial,data=TrainAutoE) #Logistic regression
LOGpred=predict(LOGAutoE,TestAutoE)
Ones=rep(1,50)
Ones[LOGpred<0.5]=0
table(Ones,TestAutoE$mpg01)   #----------(33+4)/50=74% Accuracy, 16%error
#---------------------(g)-------------------------------------------
TrainMat=cbind(TrainAutoE$horsepower,TrainAutoE$weight,TrainAutoE$acceleration)
TestMat=cbind(TestAutoE$horsepower,TestAutoE$weight,TestAutoE$acceleration)
set.seed(1)
KNNAutoE1=knn(TrainMat,TestMat,TrainAutoE$mpg01,k=1) #for k=1
table(KNNAutoE1,TestAutoE$mpg01)  # (4+36)/50=80% Accuracy, 20%error

KNNAutoE2=knn(TrainMat,TestMat,TrainAutoE$mpg01,k=2) #for k=2
table(KNNAutoE2,TestAutoE$mpg01)  # (4+35)/50=78% Accuracy, 22%error

KNNAutoE5=knn(TrainMat,TestMat,TrainAutoE$mpg01,k=1) #for k=5
table(KNNAutoE5,TestAutoE$mpg01)  # (4+36)/50=80% Accuracy, 20%error
#k=1,3,4,5 all give accuracy of 80%

KNNAutoE10=knn(TrainMat,TestMat,TrainAutoE$mpg01,k=1) #for k=10
table(KNNAutoE10,TestAutoE$mpg01)  # (4+36)/50=80% Accuracy, 20%error
#k=1,3,4,5 all give accuracy of 80%

KNNAutoE20=knn(TrainMat,TestMat,TrainAutoE$mpg01,k=1) #for k=20
table(KNNAutoE20,TestAutoE$mpg01)  # (4+36)/50=80% Accuracy, 20%error
#k=1,3,4,5 all give accuracy of 80%

KNNAutoE50=knn(TrainMat,TestMat,TrainAutoE$mpg01,k=1) #for k=50
table(KNNAutoE50,TestAutoE$mpg01)  # (4+36)/50=80% Accuracy, 20%error
#k=1,3,4,5 all give accuracy of 80%

KNNAutoE100=knn(TrainMat,TestMat,TrainAutoE$mpg01,k=1) #for k=100
table(KNNAutoE100,TestAutoE$mpg01)  # (4+36)/50=80% Accuracy, 20%error
#k=1,5,10,20,50,100 all give accuracy of 80%

