#------------------------ch-2--(9)--------------------------------------
#--9a--------------
setwd("C:/Users/Deepak/Desktop/ML-Stanford/ISLR")
auto=read.csv("Auto.csv")
View(auto)
length(auto)
dim(auto)
#Problem 9b---------------------
names(auto)
range(auto$mpg)
range(auto$cylinders)
range(auto$displacement)
range(auto$horsepower)
range(auto$weight)
range(auto$acceleration)
range(auto$year)
range(auto$origin)
range(auto$name)
#Problem 9c---------------------
mean(auto$mpg)
sd(auto$mpg)
#Problem 9d---------------------
#Now remove the 10th through 85th observations------
auto.block<-auto[10:85,]
names(auto.block)
mean(auto.block$mpg)
mean(auto.block$year)
mean(auto.block$cylinders)
mean(auto,block$weight)
sd(auto.block$mpg)
sd(auto.block$cylinders)
sd(auto.block$weight)
#Problem 9e-------------
plot(auto[,1:7])
#weight and displacement are linearly scattered.
#problem 9f-------------
#9f---mpg can be predited by dispalcement,weight,acceleration, vs mpg graphs. 