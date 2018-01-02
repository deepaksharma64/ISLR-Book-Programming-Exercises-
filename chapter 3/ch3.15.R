#---------------15-------------------
library(ISLR)
library(MASS)
data(Boston)
names(Boston)
#----------(a)----------
#--Do not know yet how loop all the features in one statement so doing it independently--
pairs(Boston)
lm.fit1=lm(crim~zn,data=Boston)
summary(lm.fit1)  #RSE=8.435,R^2=0.04019,Pr(>|t|) for zn=5.51e-06
lm.fit2=lm(crim~indus,data=Boston)
summary(lm.fit2)  #RSE=7.866,R^2=0.1653,Pr(>|t|) for indus<2e-016
lm.fit3=lm(crim~chas,data=Boston)
summary(lm.fit3)  #RSE=8.59,R^2=0.00312,Pr(>|t|) for chas=0.209
lm.fit4=lm(crim~nox,data=Boston)
summary(lm.fit4) #RSE=7.81,R^2=0.1772,Pr(>|t|) for nox<2e-16
lm.fit5=lm(crim~rm,data=Boston)
summary(lm.fit5) #RSE=8.401,R^2=0.0480,Pr(>|t|) for rm=6.35e-07
lm.fit6=lm(crim~age,data=Boston)
summary(lm.fit6) #RSE=8.057,R^2=0.1244,Pr(>|t|) for age=2.85e-16
lm.fit7=lm(crim~dis,data=Boston)
summary(lm.fit7) #RSE=7.965,R^2=0.1441,Pr(>|t|) for dis<2e-16
lm.fit8=lm(crim~rad,data=Boston)
summary(lm.fit8) #RSE=6.71,R^2=0.3913,Pr(>|t|) for rad<2e-16
lm.fit9=lm(crim~tax,data=Boston)
summary(lm.fit9) #RSE=6.997,R^2=0.3396,Pr(>|t|) for tax<2e-16
lm.fit10=lm(crim~ptratio,data=Boston)
summary(lm.fit10) #RSE=8.24,R^2=0.08407,Pr(>|t|) for ptratio=2.94e-11
lm.fit11=lm(crim~black,data=Boston)
summary(lm.fit11) #RSE=7.946,R^2=0.1483,Pr(>|t|) for black<2e-16
lm.fit12=lm(crim~lstat,data=Boston)
summary(lm.fit12) #RSE=7.664,R^2=0.206,Pr(>|t|) for lstat<2e-16
lm.fit13=lm(crim~medv,data=Boston)
summary(lm.fit13) #RSE=7.934,R^2=0.1508,Pr(>|t|) for medv<2e-16
#Pr(>|t|) for indus,nox,dis,rad,tax,black,istat,medv are low. and seems to be statistically associated with crim.
#this can be seen in the plotted figures as well.

#**********************testing same with for loop*******loop is not working***********************************
isrow<-c("age","black","chas","dis","indus","lstat","medv","nox","ptratio", "rad","rm","tax","zn")
                for(i in 1:length(isrow)){
                lm.fita[i]<-lm(crim~isrow[i],data=Boston) 
                summary(lm.fita[i])$fstatistic
} 
#**************************test over**********************************

#-----------(b)----------
lm.fit=lm(crim~.,data=Boston)
summary(lm.fit)
#Pr(>|t|) for medv,dis,rad are relatively low and null Hypothesis can be rejected.

#-----------(c)-----------
#results in (a) shows #Pr(>|t|) for medv,dis,rad,indus,nox,tax,black,istat are low. and seems to be statistically associated with crim.
#while in (b) only Pr(>|t|) for medv,dis,rad are relatively low and shows some kind of relaton with crim
yr<-summary(lm.fit)$coefficients[,1]
y=as.numeric(yr)[2:14]
x1<-as.numeric(summary(lm.fit1)$coefficients[2,1])
x2<-as.numeric(summary(lm.fit2)$coefficients[2,1])
x3<-as.numeric(summary(lm.fit3)$coefficients[2,1])
x4<-as.numeric(summary(lm.fit4)$coefficients[2,1])
x5<-as.numeric(summary(lm.fit5)$coefficients[2,1])
x6<-as.numeric(summary(lm.fit6)$coefficients[2,1])
x7<-as.numeric(summary(lm.fit7)$coefficients[2,1])
x8<-as.numeric(summary(lm.fit8)$coefficients[2,1])
x9<-as.numeric(summary(lm.fit9)$coefficients[2,1])
x10<-as.numeric(summary(lm.fit10)$coefficients[2,1])
x11<-as.numeric(summary(lm.fit11)$coefficients[2,1])
x12<-as.numeric(summary(lm.fit12)$coefficients[2,1])
x13<-as.numeric(summary(lm.fit13)$coefficients[2,1])
x<-c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)
plot(x,y,type="p",col="red")
#------------------------(d)--------------------------
#chas and nox cannot be computed for 3rd degree polynomial as they can take only two possible values in their columns.
lm.fit10=lm(crim~poly(zn,3),data=Boston)
summary(lm.fit10) 
lm.fit20=lm(crim~poly(indus,3),data=Boston)
summary(lm.fit20)  
lm.fit50=lm(crim~poly(rm,3),data=Boston)
summary(lm.fit50) 
lm.fit60=lm(crim~poly(age,3),data=Boston)
summary(lm.fit60) 
lm.fit70=lm(crim~poly(dis,3),data=Boston)
summary(lm.fit70) 
lm.fit80=lm(crim~poly(rad,3),data=Boston)
summary(lm.fit80) 
lm.fit90=lm(crim~poly(tax,3),data=Boston)
summary(lm.fit90) 
lm.fit100=lm(crim~poly(ptratio,3),data=Boston)
summary(lm.fit100) 
lm.fit110=lm(crim~poly(black,3),data=Boston)
summary(lm.fit110) 
lm.fit120=lm(crim~poly(lstat,3),data=Boston)
summary(lm.fit120) 
lm.fit130=lm(crim~poly(medv,3),data=Boston)
summary(lm.fit130)
#medv,dis and nox and few more seems to have non-linear association based on t- and f- statistics values.