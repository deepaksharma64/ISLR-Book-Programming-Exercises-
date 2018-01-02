#-------------------------(10)------------------------------------
library(MASS)
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
#--------------------------(a)------------------------------------
set.seed(10)
A=c(rnorm(20000))
Xmatrix=matrix(A,nrow=1000)  #1000x20
set.seed(20)
beta=matrix(sample(c(rnorm(14),rep(0,6))))  #20x1
set.seed(30)
err=matrix(rnorm(1000))
Y=Xmatrix%*%beta+err

na1=matrix(rep(0,20))
for(i in 1:20){
  na1[i]=as.character(i)
}
colnames(Xmatrix)=na1
colnames(Y)="y"
simdata=data.frame(cbind(Y,Xmatrix)) 
dim(simdata) #1000x21
names(simdata) # column names: y 1 2 3 4 5 6 7 8 9......20
#------------------------(b)------------------------------------
set.seed(1)
con=sample(c(rep(TRUE,100),rep(FALSE,900)))
train=simdata[con,]
test=simdata[!con,]
dim(train) #100x21
dim(test)  #900x21
#------------------------(c)---------train MSE----------------------------
sub.fit=regsubsets(y~.,data=train,nvmax=20)
summary(sub.fit)
names(sub.fit)
mseTrain=(sub.fit$rss)/nrow(train)
plot(mseTrain) #training mse(=rss/nrow(train)) decreases with increasing features
#------------------------(d)------test MSE--------------------------------------------  
#can use function mentioned in the practice labs but I will write my own...
Xtest=model.matrix(y~.,data=test)  #900x21
yhat=matrix(rep(0,18000),nrow=900)
for(i in 1:nrow(test)){
for (j in 1:ncol(Xmatrix)){
     MatA=matrix(Xtest[i,names(coef(sub.fit,id=j))])
     MatC=matrix(coef(sub.fit,id=j))
     yhat[i,j]=t(MatA)%*%MatC
                          }
}
mseTest=matrix(rep(0,18000),nrow=900)
for(i in 1:20){
mseTest[,i]=(test[,1]-yhat[,i])^2
}
meanArray=apply(mseTest,2,mean)
which.min(meanArray) #12th index MSE=1.314954
plot(meanArray)
#--------------------------------(e)---------------------------------------------
#Minimum MSE is for subset with 12 predictors. in training set MSE decrearsed 
#with increasing number of predictors but with test data it is not. This was expected
#--------------------------------(f)------------------------------------------
par(mfrow=c(2,2))
plot(yhat[,12])
hist(yhat[,12])
plot(test[,1])
hist(test[,1])
#data looks very similar......................
#--------------------------------(g)------------------------------------------
rownames(beta)<-c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20')
beta=as.data.frame(beta)
final=rep(0,20)
for(i in 1:ncol(Xmatrix)){
Bhat=as.data.frame(coef(sub.fit,id=i))
common=names(coef(sub.fit,id=i))[-1]
final[i]=sqrt((sum((beta[common,]-Bhat)^2))+sum(beta^2)-sum(beta[common,])^2)
}
par(mfrow=c(1,1))
plot(final)
#14 is minimum in this case. It goes down with increasing features. 
#Plot in d and g shows opposite but somewhat complimentary trend.