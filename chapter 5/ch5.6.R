#----------------------------6-----------------------------------------------------
library(ISLR)
library(boot)
dim(Default)   #10000x4
names(Default) # default, student, balance, income.
#------------------------------------(b)-------------------------------------------------------------
attach(Default)
set.seed(6)
boot.fn=function(data,index){
train.lr=glm(default~income+balance,data=data,family=binomial,subset=index)
return(coef(train.lr))
}
#---------------(a)------could have been written independently without writing a function------------
FunDef(Default,1:10000)
summary(boot.fn(Default,1:10000)) #std.error 4.348e-01, 4.985e-08, 2.274e-04
#------------------------------------(c)-------------------------------------------------------------
boot(Default,boot.fn,R=100) #std.error 3.609e-01,4.799e=06,1.942e-04
#------------------------------------(d)-------------------------------------------------------------
#std.error difference in bootstap and original data suggest fitted model has a scope of improvenemt.
#----------------------------------------------------------------------------------------------------


