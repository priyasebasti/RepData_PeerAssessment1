---
title: "Reproducible Research"
author: Priya Sebastian
date: August 8, 2020
output: 
  html_document:
    keep_md: true
---
  
## Background
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading the Dataset and Basic Analysis

The code below loads the libraries we need 


```r
library(dplyr)
library(rpart) 
library(rpart.plot) 
library(stats)
library(caret)
library(data.table)
library(tidyverse)
library(ggplot2)
library(lattice)
```
## Loading and preprocessing the data

##### Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv()). Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity<-read.csv('activity.csv')
```

## What is mean total number of steps taken per day?

##### Calculate the total number of steps taken per day

```r
steps_day<- aggregate(steps ~ date, activity, sum)
```
##### Make a histogram of the total number of steps taken each day

```r
hist(steps_day$steps,col="yellow", main = "Total Number of Steps Per Day", xlab = "Sum of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
##### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
steps_interval<- aggregate(steps~interval, activity, mean)
```
##### Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(steps_interval$interval,steps_interval$steps, type="l", main = "Average Steps by Interval", ylab = "Average Steps", xlab = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
##### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_interval[which.max(steps_interval$steps),1]
```

```
## [1] 835
```



## Imputing missing values

##### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
##### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
activity_2<- activity
for (i in 1:nrow(activity_2)) 
{
  if(is.na(activity_2[i,]$steps))
  {
    activity_2[i,]$steps <- if (length(subset(steps_interval,steps_interval$interval==activity_2[i,]$interval)$steps) == 0) 
                                {0}
                            else
                                {subset(steps_interval,steps_interval$interval==activity_2[i,]$interval)$steps}
                            
  }
}
```
##### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_day_2<- aggregate(steps ~ date, activity_2, sum)

hist(steps_day_2$steps,col="yellow", main = "Total Number of Steps Per Day", xlab = "Sum of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean(steps_day_2$steps)
```

```
## [1] 10766.19
```

```r
median(steps_day_2$steps)
```

```
## [1] 10766.19
```
We see that the mean hasn't changed but setting the missing values to the mean did change the median.


```r
activity_2$day <- weekdays(as.POSIXct(activity_2$date))
activity_2$day_type <- ifelse(activity_2$day %in% c("Sunday","Saturday"),"Weekend","Weekday")
```

```r
StepsNew<-aggregate(steps~interval+day_type, activity_2,mean)
xyplot(data= StepsNew, steps~interval|day_type, type = "l",layout=c(1,2),main = "Activity Patterns by Day Type", xlab = "Interval",ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
