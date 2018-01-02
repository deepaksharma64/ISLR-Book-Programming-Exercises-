#----------------ch-2--(10)--------
require(ISLR)
require(MASS)
data(Boston)
str(Boston)
names(Boston)
?Boston
class(Boston)
class(Boston$age)
#rows=506 Columns=14
#columns contains diffeent paracmeters of the city boston(in string format) and row contains numbers and integers
plot(Boston[,1:14])
--------#10c-----------------
cor(Boston$crim,Boston$rad)  #cor 0.62
cor(Boston$crim,Boston$tax)  #cor 0.58
#rad-index to accessibility to radial highway
#tax-full-value property-tax rate per\$10,000.
#----------10d-----------
top10crim<-order(Boston$crim,decreasing = T)[1:10]
top10crim
#Boston[top10crim,] #gives to 5 town by crime rate.
top10tax<-order(Boston$tax,decreasing = T)[1:10]
top10tax
#Boston[top10tax,] #gives to 5 town by crime rate.
top10ptratio<-order(Boston$ptratio,decreasing = T)[1:10]
top10ptratio
#Boston[top10ptratio,] #gives to 5 town by crime rate.
#------------10e-----------------------
sum(Boston$chas) #------------total=35----------------
#------------10f-----------------------
summary(Boston$ptratio)  #-----median 19.05--------
#------------10g-----------------------
minMed<-sort(Boston$medv)
minMed
minMedIndex<-order(Boston$medv) #min values are of 399 and 406 rows.
Boston[399,] #details of 399 row
Boston[406,] #details of 406 row
summary(Boston)  
#Mean Crim is 3.6 [399]-38 [406]-67 i.e both have high crime rates.
#--------10h-------------------------------
rmH<-sort(Boston$rm) #sorts the content in lower to higher order
order(Boston$rm) #gives the order in lower to higher 
high7<-Boston[Boston$rm>7,]
str(high7)  #......64 objects........
high8<-Boston[Boston$rm>8,]
str(high8)  #.......13 objects.......
rbind(sapply(Boston[(Boston$rm>8),],mean),sapply(Boston,mean))
#Crime rate is almost 5 times less,other parameters can be seen.
