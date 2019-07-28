#Author Subramanyam Mogili
  

#Declaring a variable x and printing it class
x<-"10"
class(x)
#Reading a liberay called "redar"
library(readr)

#Reading data from csv file and printing out its mean of observation
ozone<-read.csv(file = "/Users/smogili/Desktop/ANLY\ 506/file.csv",header=TRUE,sep = ",")
print(mean(ozone$Observation.Count))

#Reading a liberay called "tidyverse"
library(tidyverse)
#names(ozone) <- 

#Reading a liberay called "nycflights13"
library(nycflights13)
#Reading a liberay called "tidyverse"
library(tidyverse)
#Finding out the number of flights whihc are @ month 6 and 1 and day of 30.
flightData <- data.frame(flights)
flightData <- subset(flightData,flightData$month == 6 | flightData$month == 1)
flightData <- subset(flightData,flightData$day == 30)
sum(is.na(flightData$dep_delay))
#Omitting out NA's.
flightData <- na.omit(flightData, flightData$dep_delay)
mean(flightData$dep_delay)

#Reading data from csv file and printing out its mean,trimmedian and median of observation
income<-read.csv(file = "/Users/smogili/Desktop/ANLY\ 506/income.csv",header=TRUE,sep = ",")
income <- na.omit(income, income$M_weekly)
income <- na.omit(income, income$F_weekly)
mean = mean(income$M_weekly)
trimmedmean = mean(income$M_weekly, trim=0.10)
median = median(income$M_weekly)
#Comparing mean median and trimmedian
mean>median
median>trimmedmean

income$diff <- income$M_weekly - income$F_weekly
sd(income$diff, na.rm = TRUE)


library(dplyr)
library(tibble)
income <- as_tibble(income)
filterIncome <- filter(income, income$M_weekly > 2000)

weighted_median <- function(data,colName,weightName,na.rm){
  if(na.rm){
    data <- na.omit(data, colName)
    return(weighted.mean(colName,weightName))
  } 
}
weighted_median(income,income$M_weekly,income$Industry,TRUE)

#Reading mtcars
data("mtcars")
summary(mtcars)
#standard deviation of mtcars
sd(mtcars$mpg, na.rm = TRUE)
IQR(mtcars$mpg)
mad(mtcars$mpg, center = median(mtcars$mpg), constant = 1.4826, na.rm = TRUE,
    low = FALSE, high = FALSE)

#aggreating cyl of Mtcars
aggregate(mtcars$cyl,  by=mtcars, FUN=mean)

#Corelating comparision of Mtcars
cor(mtcars$disp,mtcars$cyl)
cor(mtcars$wt,mtcars$cyl)
cor(mtcars$mpg,mtcars$hp)

#creating linear model
model = lm(mtcars$mpg ~ mtcars$hp + mtcars$wt + mtcars$disp, mtcars$data)
plot(model)

library(ggplot2)
data(mpg)
is.numeric(mpg$year)
ggplot(mpg, aes(displ,hwy)) + geom_point(fill='red',color = 'black', size = 3,shape = 24)


diamonds <- data("diamonds")
summary(diamonds$color)

data("faithful")
duration = faithful$eruptions 
round(quantile(duration, c(.25)),2) 

data(who)
view(who)

library(datasets)
data(iris)
summary(iris)
plot(iris)

flights <- data("fligths")
