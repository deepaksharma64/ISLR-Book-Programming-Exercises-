#----------------------------------7-----------------------------------------
library(ISLR)
library(boot)
dim(Weekly) #----1089x8----------
names(Weekly) #--Year,Lag1,Lag2,Lag3,Lag4,Lag5,Volume,Today,Direction
set.seed(1)
#---------------------------(a)-----------------------------------------------
fit.W=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
summary(fit.W)
#---------------------------(b)-----------------------------------------------
fit.W=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial,subset=2:nrow(Weekly))
summary(fit.W)
#---------------------------(c)----------------------------------------------------
Pred1=predict(fit.W,Weekly[1,],class="response")
Mat1=rep("Down",1)
Mat1[Pred1>=0.5]="Up"
Pred1  #0.2875       #---Down since it is less than 0.5 (0.2875)
Weekly$Direction[1]  #---Down
#Observation was classified correctly......
#---------------------------(d)-----------------------------------------------------
Pred=rep(10,nrow(Weekly))
for(i in 1:nrow(Weekly)){
fit.W=glm(Direction~Lag1+Lag2,data=Weekly[-i,],family=binomial)  
Pred[i]=predict(fit.W,Weekly[i,],class="response")  #predicting ith observation.
}
PredD=rep("Down",nrow(Weekly))
PredD[Pred>=0.5]="Up"
PreD=ifelse(PredD==Weekly$Direction,0,1) #if error 1 else 0
mean(PreD)   #55%error
#---------------------------(e)----------------------------------------------------
table(PredD,Weekly$Direction)
mean(PredD==Weekly$Direction) #Accuracy is 45% i.e 55% of times error was made.
#test error is very high 55% in this case suggesting this model is not a great fit.
#-----------------------------------------------------------------------------------
