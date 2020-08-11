#1a
a <- c(1:100)
a[1] <- 1
a[2] <- 1
i=3
while( i <= 100){
  a[i] <- (a[i-1] + a[i-2] + i - 2 + i - 2)
  i=i+1
}

print(paste("The value of A100 is",a[100]))

#1b
fun<-function(m,n){
  seq1<-c(1:n)
  result1=1
  for(i in seq1){
    result1=result1*i
  }
  seq2<-c(1:m)
  result2=1
  for(j in seq2){
    result2=result2*j
  }
  if(n>m){
    seq3<-c(1:(n-m))}else{
      seq3<-c(1:(m-n))
    }
  result3=1
  for(k in seq3){
    result3=result3*k
  }
  result=result1/(result2*result3)
  print(paste("The final result is",result))
}

m=44
n=88
fun(m,n)

#1c
lcf<-function(x,y){
  if(x>y){
    upperlimit=y
  }else{
    upperlimit=x
  }
  for(i in 1:upperlimit){
    if((x%%i==0)&&(y%%i==0)){
      result=i
    }
  }
  print(paste("The largest common factor of",x,"and",y,"is",result))
}

x=12306
y=32148
lcf(x,y)

#finding the least common multiple
lcm<-function(a,b){
  if(a>b){
    greater=a
  }else{
    greater=b
  }
  while(TRUE){
    if((greater%%a==0)&&(greater%%b==0)){
      lcm=greater
      #To terminate the loop once the condition is satisfied
      ##in case it continues to print more multiples even if the least one is already found
      break
    }
    greater=greater+1
  }
  return(lcm)
  print(paste("the least common multiple of",a,"and",b, "is",lcm(a,b)))
}
a=12306
b=32148
print(paste("the least common multiple of",a,"and",b, "is",lcm(a,b)))

#2a
#loading the file into R
WHO<-read.csv("C:/Users/123/Desktop/WHO.csv")
names(WHO)
summary(WHO)
print("The variables that have at least three missing values include FertilityRate,CellularSubscribers,LiteracyRate,GNI,PrimarySchoolEnrollmentMale and PrimarySchoolEnrollmentFemale.")

#2b
#highest fertility rate
WHO$Country[which.max(WHO$FertilityRate)]
print("The country with the highest fertility rate is Niger")
#lowest fertility rate
WHO$Country[which.min(WHO$FertilityRate)]
print("the country with the lowest fertility rate is Bosnia and Herzegovina")

#2c
tapply(WHO$GNI,WHO$Region,sd,na.rm=TRUE)
print("South-East Asia has the minimum variation in Gross National Income")
print("The standard deviation of GNI in South-East Asia is 2477.34")

#2d
#subsetting the rows of only rich countries
RichCountry<-subset(WHO,GNI>20000,na.rm=TRUE)
mean(RichCountry$ChildMortality,na.rm=TRUE)
print("The mean child mortality of the rich countries is 7.448649.")

#2e
plot(WHO$GNI,WHO$LifeExpectancy,xlab = "Income Level",ylab="Life Expectancy",main="Income Level and Life Expectancy")




