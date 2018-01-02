#---------------------------------Chapter-2--(8)-----------------------------------------
#Problem8a
setwd("C:/Users/Deepak/Desktop/ML-Stanford/ISLR")
college=read.csv("College.csv")
#Problem8b
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)
#Problem8c
summary(college)
dim(college)
names(college)
pairs(college[,1:10])  #Note- Its giving error when--Private---isinput????
plot(college$Outstate,college$Apps) #---Private is giving error----
#Problem8c-iv
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
summary(college)
#Problem-8 part v---------------
plot(college$Outstate,college$Elite)
names(college)
str(college) #Alternative to summary command#
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Top10perc)
