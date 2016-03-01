---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
keep_md: true
---

## Loading and preprocessing the data  
  
Load Activity Monitoring dataset and view dataset summary:  

```r
activity <- read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Convert "date" variable to date format:

```r
activity$date <- as.Date(as.character(activity$date))
```

## What is mean total number of steps taken per day?

Aggregate total number of steps by day:

```r
aggregated_activity <- aggregate(steps~date, data = activity,FUN = "sum" )
```

Create histogram of the total number of steps taken each day:

```r
hist(aggregated_activity$steps, col = "blue", main = "Histogram of Number of Daily Steps", xlab = "Number of steps per day", ylim = c(0,30))
abline(v = median(aggregated_activity$steps), col = "red", lwd = 3)
```

![plot of chunk histogram](figure/histogram-1.png) 

Calculate the mean and median of the total steps taken each day:

```r
print(mean <- mean(aggregated_activity$steps))
```

```
## [1] 10766.19
```

```r
print(median <- median(aggregated_activity$steps))
```

```
## [1] 10765
```
- *The average total daily number of steps is 1.0766189 &times; 10<sup>4</sup>.*
- *The median total daily number of steps is 10765.*

## What is the average daily activity pattern?

Compute average number of steps per interval, averaged accross all days :

```r
steps_per_interval <- aggregate(steps~interval, data = activity,FUN = "mean" )
```

Plot average number of steps against interval:  

```r
with(steps_per_interval,plot(interval,steps, type = "l", 
                             main = "Average Daily Activity Pattern", col = "blue"))
```

![plot of chunk plot](figure/plot-1.png) 

Find the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps:  

```r
print(max <- steps_per_interval$interval[which.max(steps_per_interval$steps)])
```

```
## [1] 835
```
  
*The 5-minute interval that contains the maximum number of steps is interval 835.*

## Imputing missing values

Calculate the total number of missing values in the dataset:

```r
print(na_table <- table(is.na(activity$steps)))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
na_vals <- na_table[2]
```
*There are 2304 missing values in the dataset.*

Replace NA values:  

*Where step values are missing, we are going to replace the NA values by the average number of steps on the given 5-minute interval. We are using this strategy because intuitively and as confirmed by the Average Daily Pattern plot, the number of step seems to vary greatly with the time of the day.* 


```r
activity_clean <- activity
n <- dim(activity_clean)[1]

for (i in 1:n) {
  if (is.na((activity_clean$steps)[i])) {
    na_entry <- activity[i,]
    na_entry_interval <- na_entry[3]
    j <- match(na_entry_interval,steps_per_interval$interval)
    activity_clean$steps[i] <- steps_per_interval$steps[j]
  }
}

summary(activity_clean)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

Aggregate total number of steps by day, after NA replacement:

```r
aggregated_activity_clean <- aggregate(steps~date, data = activity_clean, FUN = "sum" )
```


Create a new histogram of the total number of steps taken each day, after NA replacement:

```r
hist(aggregated_activity_clean$steps, col = "blue", main = "Histogram of Number of Daily Steps (clean)", xlab = "Number of steps per day", ylim = c(0,30))
abline(v = median(aggregated_activity_clean$steps), col = "red", lwd = 3)
```

![plot of chunk histogram_clean](figure/histogram_clean-1.png) 

Calculate the new mean and median of the total steps taken each day, after NA replacement:

```r
print(mean <- mean(aggregated_activity_clean$steps))
```

```
## [1] 10766.19
```

```r
print(median <- median(aggregated_activity_clean$steps))
```

```
## [1] 10766.19
```
- *The average total daily number of steps after NA value replacement is 1.0766189 &times; 10<sup>4</sup>.*
- *The median total daily number of steps after NA value replacement is 1.0766189 &times; 10<sup>4</sup>.*  

- *The average number of daily steps is the same in the clean activity dataset than in the original one. As a matter of fact, the missing values were ignored in the aggregating process for the original dataset and were replaced precisely by the average number of steps per interval in the new one, so the average remains unchanged.*  

- *In the clean dataset, the median number of daily steps is slightly higher than in the old one, and is equal to the mean. It is explained by the fact that our strategy for null value replacement has recentered the dataset on the mean.*

## Are there differences in activity patterns between weekdays and weekends?

Create a new variable which describe whether the day is a week-day or week-end:


```r
day_of_week <- weekdays(activity_clean$date)
activity_clean$weekday_or_weekend <- 0
n <- length(day_of_week)

for (i in 1:n) {
  if (day_of_week[i] %in% c("Saturday", "Sunday")) {
    activity_clean$weekday_or_weekend[i] <- "weekend"
  }
    else {
      activity_clean$weekday_or_weekend[i] <- "weekday"
    }
}

activity_clean$weekday_or_weekend <- as.factor(activity_clean$weekday_or_weekend)
summary(activity_clean)
```

```
##      steps             date               interval      weekday_or_weekend
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   weekday:12960     
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   weekend: 4608     
##  Median :  0.00   Median :2012-10-31   Median :1177.5                     
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

Compute average number of steps per interval, averaged across all weekday days or weekend days :

```r
activity_weekday <- subset(activity_clean, weekday_or_weekend == "weekday")
activity_weekend <- subset(activity_clean, weekday_or_weekend == "weekend")

steps_per_interval_weekday <- aggregate(steps~interval, data = activity_weekday,
                                        FUN = "mean" )
steps_per_interval_weekend <- aggregate(steps~interval, data = activity_weekend,
                                        FUN = "mean" )
```

Plot average number of steps against interval for both weekdays and weekend:

```r
par(mfrow = c(2, 1), mar = c(4,2,2,2))
with(steps_per_interval_weekday,plot(interval,steps, type = "l", 
                             main = "weekday", col = "blue", ylim = c(0,250)))
with(steps_per_interval_weekend,plot(interval,steps, type = "l", 
                             main = "weekend", col = "blue", ylim = c(0,250)))
```

![plot of chunk plot_weekday_vs_week_end](figure/plot_weekday_vs_week_end-1.png) 


*We can see that the pattern is quite different whether we're on a weekday or a weekend day.*
