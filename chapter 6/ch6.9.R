#-------------------------(9)-------------------------------
library(MASS)
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
#-------------------------(a)--------------------------------
names(College) #1.Private,2.Apps,3.Accept,4.Enroll,5.Top10perc,6.Top25perc,7.F.Undergrad,8.P.Undergrad,9.Outstate,10..Room.Board,11.Books,12.Personal,13.PhD,14.Terminal,15.S.F.Ratio,16.perc.alumni,17.Expend,18.Grad.Rate 
dim(College) #777x18
set.seed(10)
A= sample(c(TRUE,FALSE),777,replace = TRUE,prob =c(2,1))
train=College[A,]
test =College[!A,]
dim(train) #504x18
dim(test) #273x18
#-------------------------(b)-----linear model-----------------------------
linearM=lm(Apps~.,data=College)
summary(linearM) #RSE=1041 #AR2=0.9276,p-value=2.2e-16
Py=predict(linearM,newdata = test)
mseS=mean((Py-test$Apps)^2) #903827.8
#--------------------------(c)----Ridge Resgression-------------------------
X=model.matrix(Apps~.,College)[,-1]
Y=College$Apps
Xtrain=model.matrix(Apps~.,train)[,-1]
Xtest=model.matrix(Apps~.,test)[,-1]

Rid.fit=glmnet(Xtrain,train$Apps,alpha=0)
set.seed(5)
Rid.cv=cv.glmnet(Xtrain,train$Apps,alpha=0)
OptlambdaR=Rid.cv$lambda.min #436.5348
Yhat=predict(Rid.fit,s=OptlambdaR,newx = Xtest)
mseR=mean((test$Apps-Yhat)^2) #912308.23
Rid.fitXY=glmnet(X,Y,alpha=0)
coefficients(Rid.fitXY,OptlambdaR) 
#----------------------------(d)----Lasso-----------------------------------
Las.fit=glmnet(Xtrain,train$Apps,alpha=1)
plot(Las.fit)
set.seed(10)
Las.cv=cv.glmnet(Xtrain,train$Apps,alpha=1)
OptlambdaL=Las.cv$lambda.min #2.55
Yhat=predict(Las.fit,s=OptlambdaL,newx = Xtest)
mseL=mean((test$Apps-Yhat)^2) #1035070
Las.fitXY=glmnet(X,Y,alpha=1)
CoefLass=coefficients(Las.fitXY,OptlambdaL)
length(CoefLass[CoefLass==0]) #Is not shrinking any value to 0 beacuse selected lambda is too low in cv.glmnet
#----------------------------(e)----PCR-----------------------------------------
set.seed(2)
pcr.fit.train=pcr(Apps~.,data=train,scale=TRUE,validation="CV")
summary(pcr.fit.train)
validationplot(pcr.fit.train,val.type="MSEP")
YhatP=predict(pcr.fit.train,Xtest,ncomp=10) #minimum for 10 componenets
msePCR=mean((test$Apps-YhatP)^2) #1270875 high
#----------------------------(f)----PLS----------------------------------------
set.seed(2)
pls.fit.train=plsr(Apps~.,data=train,scale=TRUE,validation="CV")
summary(pls.fit.train)
validationplot(pls.fit.train,val.type="MSEP")
YhatPls=predict(pls.fit.train,Xtest,ncomp=11) #minimum for 11 components
msePLS=mean((test$Apps-YhatPls)^2) #1047421 high
#-----------------------------(g)-----------------------------------------------
#lowest test error is in case of least square estimate followed by Ridge,Lasso,PLS and PCR. All are very close though.
#-------------------------------------------------------------------------------
