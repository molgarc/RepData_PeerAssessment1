---
title: 'Reproducible Research: Peer Assessment Week 2'
author: "Mariano Molina García"
date: "March 25, 2020"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data
Show any code that is needed to Load the data and process/transform the data (if necessary) into a format suitable for your analysis.

1. Load the data


```r
setwd("C:/EMNOMIA/Data Science Specialization/Data_Science_Repo/RepData_PeerAssessment1")
info_activity = read.csv("./repdata_data_activity/activity.csv")
```

2. Info about data


```r
summary(info_activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
names(info_activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
dim(info_activity)
```

```
## [1] 17568     3
```

```r
head(info_activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(info_activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
pairs(info_activity)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



## What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day. 


```r
totalStepsPerDay <- aggregate(steps ~ date, info_activity, sum, na.rm=TRUE)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
hist(totalStepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanStepsPerDay <- mean(totalStepsPerDay$steps)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
medianStepsPerDay <- median(totalStepsPerDay$steps)
medianStepsPerDay
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 

```r
avgStepsPerInterval<-aggregate(steps~interval, data=info_activity, mean, na.rm=TRUE)
plot(steps~interval, data=avgStepsPerInterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalMaxSteps <- avgStepsPerInterval[which.max(avgStepsPerInterval$steps),]$interval
intervalMaxSteps
```

```
## [1] 835
```

## Imputing missing values

1. Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
numberNAInfoActivity <- sum(is.na(info_activity$steps))
numberNAInfoActivity 
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
info_activity_withoutNaN <- info_activity
for(x in 1:nrow(info_activity_withoutNaN)) {
    if(is.na(info_activity_withoutNaN[x,]$steps)) {
        info_activity_withoutNaN[x,]$steps <- avgStepsPerInterval[
            avgStepsPerInterval$interval%in% info_activity_withoutNaN[x,]$interval,]$steps
    }
}
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsPerDayWithoutNA <- aggregate(steps ~ date, data=info_activity_withoutNaN, sum)
hist(totalStepsPerDayWithoutNA$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
1. For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
info_activity_withoutNaNDayInfo<-info_activity_withoutNaN
info_activity_withoutNaNDayInfo$date <- 
    as.Date(info_activity_withoutNaNDayInfo$date, format = "%Y-%m-%d")
info_activity_withoutNaNDayInfo$days <- weekdays(info_activity_withoutNaNDayInfo$date)
dayWeekend = c("Saturday","Sunday")

for(x in 1:nrow(info_activity_withoutNaNDayInfo)) {
    if(info_activity_withoutNaNDayInfo[x,]$days %in% dayWeekend){
    info_activity_withoutNaNDayInfo[x,]$days <- "Weekend(Saturday and Sunday)"
    }
    else{
        info_activity_withoutNaNDayInfo[x,]$days <- "Weekday(Monday to Friday)"
    }
}
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
info_activity_withoutNaNDayInfoAveraged <-aggregate(
    steps~interval+days, data=info_activity_withoutNaNDayInfo, mean, na.rm=TRUE)
library(lattice)
xyplot(steps ~ interval | days, data = info_activity_withoutNaNDayInfoAveraged, type = "l",
       layout = c(1, 2),xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
