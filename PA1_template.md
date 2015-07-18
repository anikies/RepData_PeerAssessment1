# Reproducible Research: Peer Assessment 1
####Loading and preprocessing the data
1. #####Load the data (i.e. read.csv())

```r
activity <- read.csv("activity.csv")
```

2. #####Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.character(activity$date)
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

####What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. #####Calculate the total number of steps taken per day


```r
activitymeandate <- aggregate(steps ~ date, data=activity, FUN=mean,na.rm=T)
```

2. #####If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(activitymeandate$steps,main="Histogram total steps taken each day",xlab="steps", ylim=c(0,20))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3. #####Calculate and report the mean and median of the total number of steps taken per day

```r
mean(activitymeandate$steps)
```

```
## [1] 37.3826
```

```r
median(activitymeandate$steps)
```

```
## [1] 37.37847
```
####What is the average daily activity pattern?

1. #####Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activitymeansteps <- aggregate(steps ~ interval, data=activity, FUN=mean,na.rm=T)
plot(activitymeansteps$interval,activitymeansteps$steps, type="l",main="Time series plot", xlab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. #####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
x <- na.omit(activity[activity$steps == max(activity$steps,na.rm = T), ])[,1]
```

####Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. #####Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

2. #####Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
activitycomplete <- activity

for (i in 1:nrow(activity)){
  if (is.na(activitycomplete[i,1])){
      activitycomplete[i,1] =  trunc(activitymeansteps[activitymeansteps$interval == activity[i,3],2])
      #activitycomplete[i,1] =  trunc(activitymeandate[activitymeandate$date == activity[i,2],2])
  }
}
```

3. #####Create a new dataset that is equal to the original dataset but with the missing data filled in.

* mydatacomplete

4. #####Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
  activitycompletemeandate <- aggregate(steps ~ date, data=activitycomplete, FUN=mean,na.rm=T)
  hist(activitycompletemeandate$steps,main="Histogram total steps taken each day (no NA)",xlab="steps", ylim=c(0,30)) 
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
  mean(activitycompletemeandate$steps)
```

```
## [1] 37.32559
```

```r
  median(activitycompletemeandate$steps)
```

```
## [1] 36.94792
```
There is no impact because I fill the NA values with the mean of the group

####Are there differences in activity patterns between weekdays and weekends?

There is more activity during the weekends. 
During the week, there is a spike at around 7.50 (going to work)  

  For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. #####Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
  library(lubridate)
  activitycomplete$weekday[wday(activity$date) %in% c(2, 3, 4, 5, 6)] <- "weekday"
  activitycomplete$weekday[wday(activity$date) %in% c(1,7 )] <- "weekend"
  activitycomplete$weekday <- as.factor(activitycomplete$weekday)
```

3. #####Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
  activitycompletemeanday <- aggregate(steps ~ interval + weekday, data=activitycomplete, FUN=mean,na.rm=T)
  library(lattice)
  xyplot(steps ~ interval | weekday, activitycompletemeanday, type = "l", layout = c(1, 2),ylab="Number of steps",xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
  meanstepsweekend <- aggregate(steps ~ weekday, data=activitycomplete, FUN=mean,na.rm=T)
```

