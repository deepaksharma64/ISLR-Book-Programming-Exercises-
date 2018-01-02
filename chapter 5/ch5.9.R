#-------------------------9----------------------------------------
library(ISLR)
library(MASS)
library(boot)
dim(Boston) #--506x14--
names(Boston) #crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv 
#-----------------------(a)----------------------------------------
u_hat=mean(Boston$medv) #22.53280 Population mean of medv
#-----------------------(b)----------------------------------------
sd_err=sd(Boston$medv)/sqrt(nrow(Boston))  #0.4088611 sd error of mean---
#sd error is measure of how accrate is the computed sample mean when compared to the orignal population mean.
#-----------------------(c)----------------------------------------
meanf=function(data,index){
  u_hatB=mean(data[index])
  return(u_hatB)
}
meanf(Boston$medv,1:nrow(Boston))
set.seed(1)
u_hatBoot=boot(Boston$medv,meanf,R=1000)  #22.53281 mean,std.error=0.41193
#-----------------------(d)---------------------------------------------------
conf95BootL= u_hatBoot$t0-2*sd(u_hatBoot$t) # 21.70
conf95BootU= u_hatBoot$t0+2*sd(u_hatBoot$t) # 23.35
# 95% conf interval [21.70,23.35]
#------------------------(e)--------------------------------------------------
med_hat=median(Boston$medv)               #21.2
#------------------------(f)--------------------------------------------------
medf=function(data,index){
  med_hatB=median(data[index])
  return(med_hatB)
}
medf(Boston$medv,1:nrow(Boston))
set.seed(2)
med_hatBoot=boot(Boston$medv,medf,R=1000)
med.std.err=sd(med_hatBoot$t)   #0.3821 std.err of median 
#std.error of mean and median is very close to each other and values of mean and median is also very similar. This suggest data is evenly distributed about the mean on both sides.
#---------------------------(g)---------------------------------------------------
u01_hat=quantile(Boston$medv,0.10) #10% is 12.75
#---------------------------(h)------------------------------------------------
mf=function(data,index){
  u01_hat=quantile(data[index],0.10)
  return(u01_hat)
}
set.seed(1)
u01_hatBoot=boot(Boston$medv,mf,R=1000)  
# std.error is 0.5050 which is reasonable.  
#-----------------------------------------------------------------------------------------------------------------------------


