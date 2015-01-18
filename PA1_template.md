# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
This code reads the data into R, and creates a version of the data which omits incomplete cases for the initial questions.


```r
library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: splines
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
activity<-read.csv("activity.csv")
activity$interval<-as.factor(activity$interval)
activity$date<-as.Date(activity$date)
activityomit<-na.omit(activity)
```

## What is mean total number of steps taken per day?
This code determines the sum of steps taken each day, creates a histogram of this data, and computes the mean and median of steps taken each day.


```r
sumbyday<-tapply(activityomit[,1],activityomit[,2],sum)
hist(sumbyday,nrow(sumbyday),main="Histogram of Number of Steps by Date")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(sumbyday,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sumbyday,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
This code plots the average number of steps in each interval across all days, as well as the "busiest" (max) interval.


```r
meanbyinterval<-tapply(activityomit[,1],activityomit[,3],mean)
plot(meanbyinterval,xlab="Interval",ylab="Steps",main="Mean Steps by Interval", type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
max(meanbyinterval)
```

```
## [1] 206.1698
```

This histogramn is consistent with a typical schedule. There are few steps taken in the late/night early morning hours when one usually sleeps, and spikes that may be associated with commutes in the morning and evening.

## Imputing missing values
This code imputes the missing values in the original data. A very simple strategy was used. The average number of steps taken across all days and intervals was used to fill in all NA values. A histogram of the sum of steps each day is presented with this data set with imputed missing values.


```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
activity.i<-activity
activity.i$imputed_steps <- with(activity.i, impute(steps,mean))

sumbyday.i<-tapply(activity.i$imputed_steps,activity.i$date,sum)
hist(sumbyday.i,nrow(sumbyday.i))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(sumbyday.i)
```

```
## [1] 10766.19
```

```r
median(sumbyday.i)
```

```
## [1] 10766.19
```

The mean in this data set is the same as the mean in the original data set, while the median differs slightly, and is now equal to the mean. This is because the strategy used to impute missing values was to input the average across all days. This would not effect the mean, and would create a many values that are equal to the average, making the median fall among those.

## Are there differences in activity patterns between weekdays and weekends?
This code identifies which days are weekends and weekdays, determines the average steps in each interval for both weekends and weekdays, then presents plots to compare them.


```r
activity.i$day<-weekdays(activity.i$date)
for(i in 1:nrow(activity.i)){
    if(activity.i[i,5]=="Saturday" | activity.i[i,5]=="Sunday") {
        activity.i[i,5]<-"Weekend"
    } else {
        activity.i[i,5]<-"Weekday"
    }
}

activity.i$day<-as.factor(activity.i$day)
tapply(activity.i$imputed_steps,activity.i$day,sum)
```

```
##  Weekday  Weekend 
## 461513.1 195224.4
```

```r
activity.e<-activity.i[activity.i$day=="Weekend",]
activity.w<-activity.i[activity.i$day=="Weekday",]
meanbyinterval.e<-tapply(activity.e[,4],activity.e[,3],mean)
meanbyinterval.w<-tapply(activity.w[,4],activity.w[,3],mean)

plot(meanbyinterval.e,xlab="Interval",ylab="Steps",main="Mean Steps by Interval on Weekends", type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
plot(meanbyinterval.w,xlab="Interval",ylab="Steps",main="Mean Steps by Interval on Weekdays", type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-2.png) 
