#-------------------------ch-8--------(7)---------------------------------
library(ISLR)
library(tree)
library(dplyr)
library(ggplot2)
library(randomForest)
library(gbm)
library(reshape2)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)   #506x14
boston.test=Boston[-train,"medv"]
test.er=matrix(0,nrow=500,ncol=3)
for(j in 1:3){
for (i in 1:500){
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=(j*3+1),importance=TRUE,ntree=i) #mtry=4,7,10
rf.boston
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
test.er[i,j]=mean((yhat.rf-boston.test)^2)
}
}
Xtree=c(1:500)
test.err=data.frame(Xtree,test.er)

#plotting without ggplot-----------------------
attach(test.err)
x<-cbind(Xtree,Xtree,Xtree)
y<-cbind(X1,X2,X3)
matplot(x,y,type="l",xlab = "Number of Trees", ylab = "Test Classification Error")
legend(500,2, legend=c("m=4", "m=7","m=10"),lty=1:2, cex=0.8)
#plotting with ggplot--------------------------
#when you melt essentially you create only one column with the value
#and one column with the variables.
d <- melt(test.err, id.vars="Xtree",lwd=2)
ggplot(d,aes(Xtree,value,col=variable))+
theme_bw()+
geom_line()+
labs(x="Number of Trees",y="Test Classification Error",title="Random Forest Analysis")+
scale_shape_discrete(breaks=c("X1", "X2", "X3"),labels=c("m=4", "m=7", "m=10"))+
scale_color_discrete(breaks=c("X1", "X2", "X3"),labels=c("m=4", "m=7", "m=10"))+
theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------------------



