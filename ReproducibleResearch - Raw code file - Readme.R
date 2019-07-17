## Check the current working Directory and set it correctly
getwd()
setwd('./R/Reproducible Research/')

# download file from web into the working directory
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
# unzip data and read 
unzip("activity.zip")

################### 1. What is mean total number of steps taken per day?
########## 1.1 Calculate total number of steps taken each day

stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)

# Load the required libraries
library(magrittr)
library(dplyr)

#Create a histogram and save it in .png format
png("Histogram of Total Steps by day.png")

########## 1.2. Make a histogram of the total number of steps taken each day

# Histogram of Total Steps by Day
databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)

dev.off()

########## 1.3. Calculate and report the mean and median of the total number of steps taken per day

mean(databydate$tsteps)
median(databydate$tsteps)

#################### 2. What is the average daily activity pattern?
########## 2.1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) 
########## and the average number of steps taken, averaged across all days (y-axis)

library(ggplot2)

png("Time Series Plot.png")

databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()

dev.off()

########## 2.2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]

#################### 3. Imputting Missing Values
########## 3.1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

# generate listing of NA's
missingVals <- sum(is.na(data))

missingVals

########## 3.2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
########## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 

library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

########## 3.3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)

########## 3.4. Make a histogram of the total number of steps taken each day and Calculate 
########## and report the mean and median total number of steps taken per day. 
########## Do these values differ from the estimates from the first part of the assignment? 
########## What is the impact of imputing missing data on the estimates of the total daily number of steps?

FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)

summary(FullSummedDataByDay)

png("Total Daily Steps.png")
hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
dev.off()

#################### 4. Are there differences in activity patterns between weekdays and weekends?

########## 4.1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
########## indicating whether a given date is a weekday or weekend day.

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )  

library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

png("Comparison of Avg No of Steps.png")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")

dev.off()

##########################   The End ############################
