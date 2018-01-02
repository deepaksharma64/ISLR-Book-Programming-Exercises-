#---------------------5---------------------------------------------------------
library(ISLR)
dim(Default)   #10000x4
names(Default) # default, student, balance, income.
#-----------------------------------(a)-------------------------------------------
attach(Default)
lr=glm(default~income+balance,data=Default,family=binomial)
pred=predict(lr,Default)
#-----------------------------------(b)-------------------------------------------
set.seed(100)
train=sample(10000,7000)
train.lr=glm(default~income+balance,data=Default,family=binomial,subset=train)
fit.lr=predict(train.lr,Default[-train,],type="response")
A=dim(Default[-train,])[1]
matDef=rep("No",A)
matDef[fit.lr>=0.5]="Yes"
table(matDef,Default[-train,1])
mean(matDef==Default[-train,1]) #0.974 Accuracy.Validation test error=0.026
#---------------------------------- (c)-------------------------------------------
set.seed(1)
train=sample(10000,7000)
train.lr=glm(default~income+balance,data=Default,family=binomial,subset=train)
fit.lr=predict(train.lr,Default[-train,],type="response")
A=dim(Default[-train,])[1]
matDef=rep("No",A)
matDef[fit.lr>=0.5]="Yes"
table(matDef,Default[-train,1])
mean(matDef==Default[-train,1]) #0.972 Accuracy Validation test error=0.028

set.seed(2)
train=sample(10000,7000)
train.lr=glm(default~income+balance,data=Default,family=binomial,subset=train)
fit.lr=predict(train.lr,Default[-train,],type="response")
A=dim(Default[-train,])[1]
matDef=rep("No",A)
matDef[fit.lr>=0.5]="Yes"
table(matDef,Default[-train,1])
mean(matDef==Default[-train,1]) #0.976 Accuracy. Validation test error=0.024

set.seed(3)
train=sample(10000,7000)
train.lr=glm(default~income+balance,data=Default,family=binomial,subset=train)
fit.lr=predict(train.lr,Default[-train,],type="response")
A=dim(Default[-train,])[1]
matDef=rep("No",A)
matDef[fit.lr>=0.5]="Yes"
table(matDef,Default[-train,1])
mean(matDef==Default[-train,1])#0.977 Accuracy. Validation test error=0.023
#All the Validation test errors are very close to each other.
#----------------------------(d)----------------------------------------------
set.seed(4)
train=sample(10000,7000)
train.lr=glm(default~income+balance+student,data=Default,family=binomial,subset=train)
fit.lr=predict(train.lr,Default[-train,],type="response")
A=dim(Default[-train,])[1]
matDef=rep("No",A)
matDef[fit.lr>=0.5]="Yes"
table(matDef,Default[-train,1])
mean(matDef==Default[-train,1])#0.974 Accuracy. Validation test error=0.026
#student does not look to effect Validation test errors.
#-----------------------------------------------------------------------------
