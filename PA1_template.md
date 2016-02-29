Reproducible Research: Course Project/Assignment 1
========================================

## Loading and preprocessing the data

```r
unzip("repdata-data-activity.zip")
```

```
## Warning in unzip("repdata-data-activity.zip"): error 1 in extracting from
## zip file
```

```r
activity = read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

## What is the mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
stepsperdate = aggregate(steps ~ date, data=activity, FUN=sum)
barplot(stepsperdate$steps, names.arg=stepsperdate$date, xlab="Date", ylab="Steps", border = NA, col = "tomato2")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

2. Calculate and report the **mean** and **median** total number of
   steps taken per day


```r
mean(stepsperdate$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsperdate$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
stepsperinterval = aggregate(steps ~ interval, data=activity, FUN=mean)
plot(stepsperinterval, type="l", col = "tomato2", lwd = 2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

2. Which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
stepsperinterval$interval[which.max(stepsperinterval$steps)]
```

```
## [1] 835
```


## Imput missing values

1. Calculate and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
nrow(activity) - nrow(complete.cases(activity))
```

```
## numeric(0)
```

2. Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.

The mean for the 5-minute intervals ato fill the missing
values in the dataset.

3. Create a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
activity = merge(activity, stepsperinterval, by="interval", suffixes=c("",".y"))
nas = is.na(activity$steps)
activity$steps[nas] = activity$steps.y[nas]
activity = activity[,c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. Do these values differ from the estimates from
   the first part of the assignment? What is the impact of imputing
   missing data on the estimates of the total daily number of steps?


```r
steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps", border = NA, col = "tomato2")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
mean(steps.date$steps)
```

```
## [1] 10766.19
```

```r
median(steps.date$steps)
```

```
## [1] 10766.19
```

The influence of the missing data seems low, especially when
estimating the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
findweekend = function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) 
	{
        "weekend"
  } else {
        "weekday"
  }
}
activity$Daytype = as.factor(sapply(activity$date, findweekend))
```

2. Make a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
par(mfrow=c(2,1), mar = c(4, 4, 1,1), mgp = c(1.5, 0.5, 0))
for (type in c("weekend", "weekday")) 
{
    stepstype = aggregate(steps ~ interval, data=activity, 
	                         subset=activity$Daytype==type,
							             FUN=mean)
    plot(stepstype, type ="l", main=type, col = "tomato2", lwd = 1.5)
}
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
