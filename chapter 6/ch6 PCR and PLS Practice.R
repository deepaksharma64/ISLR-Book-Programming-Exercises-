#---------ch-6 Practice PCR and PLS Regression-----------------------
library(MASS)
library(ISLR)
library(pls)
names(Hitters)
dim(Hitters) #322x20
Hitters=na.omit(Hitters)
dim(Hitters) #263x20

set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
Vplot=validationplot(pcr.fit,val.type="MSEP")

set.seed(2)
Train=sample(c(TRUE,FALSE),nrow(Hitters),replace = TRUE)
Test=!Train

pcr.fit.train=pcr(Salary~.,data=Hitters[Train,],scale=TRUE,validation="CV")
Vplot.train=validationplot(pcr.fit.train,val.type="MSEP") #Minimum at M=7
summary(pcr.fit.train) 

pcr.pred=predict(pcr.fit ,Hitters[Test,], ncomp =7)
mean((pcr.pred -Hitters$Salary[Test])^2) #100495

x=model.matrix(Salary~.,Hitters)[,-1] #remove the first col as it adds intercept automatically
y=Hitters$Salary
pcr.pred=predict(pcr.fit ,x[Test,],ncomp =7)
mean((pcr.pred -y[Test])^2) #100495

pcr.fitX=pcr(y~x,scale=TRUE,ncom=7)
summary(pcr.fitX)
pcr.predX=predict(pcr.fitX ,x[Test,],ncomp =7)
mean((pcr.predX -y[Test])^2) #100495
#-------------------------PLS------------------------------------------------
set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters[Train,],scale=TRUE,validation="CV")
Vplot=validationplot(pls.fit,val.type="MSEP")
summary(pls.fit)
pls.predX=predict(pls.fit,x[Test,],ncomp=2)
mean((pls.predX -y[Test])^2) #105648

pls.fitF=plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fitF)
#-----------------------------------------------------------------------------
