# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data <- read.csv("activity.csv",
                header=TRUE,
                sep=",",
                na.strings="NA")
#Convert to Date
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
#Remove from data rows without steps data
dataTidy <- data[!is.na(data$steps), ]
```

Adding tool functions:


```r
convert.interval <- function(interval) {
    sprintf("%02d:%02d:00", floor(interval/60),
             (interval - floor(interval/60)*60))
}
convert.weekday <- function(day) {
    if (weekdays(day) == "Sunday" | weekdays(day) == "Saturday") {
        return ("weekend")
    }
    return ("weekday")
}
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken per day


```r
sumStep = aggregate(steps~date,
                    dataTidy,
                    sum)

hist(sumStep$steps,
     col="red",
     main="Total number of steps per day",
     xlab="Steps Per Day")
```

![](PA1_template_files/figure-html/q1-1.png) 

```r
meanStep <- mean(sumStep$steps)
medianStep <- median(sumStep$steps)
```
- The **mean** of steps per days: *10766.1886792*
- The **median** of steps per days: *10765*

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgStep = aggregate(steps~interval, dataTidy, mean)

plot(avgStep$step,
     type="l",
     xaxt="n",
     xlab="5-minute interval identifiers",
     main="Average number of steps across all days",
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/q2-1.png) 

```r
#check which 5-minute interval contains the maximum number of steps
maxIdx <- which.max(avgStep$steps)
maxInterval <- avgStep[maxIdx, 'interval']
maxAvgStep <- avgStep[maxIdx, 'steps']
```
- 5-minute interval number: 835
- Time of day: from 13:55:00 till 14:00:00
- Maximum number of steps per interval: 206.17

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#calulate the total number of missing values in the dataset
totalNA <- sum(is.na(data$steps))
```
- The **total number** of missing values in the dataset: __2304__


```r
# strategy : assign 5-minute average to NAs
naIdx <- which(is.na(data$steps))
dataNew <- data
dataNew[naIdx, 'steps'] <- sapply(data[naIdx, 3],
                                 function(x) {
                                     floor(avgStep[(avgStep$interval==x), 2])
                                     }
                                 )

sumStepNew <- aggregate(steps~date, dataNew, sum)

hist(sumStepNew$steps,
     col="red",
     main="Total number of steps taken each day", xlab="Total Number of Steps")
```

![](PA1_template_files/figure-html/q3_2-1.png) 

```r
meanStepNew <- mean(sumStepNew$steps)
medianStepNew <- median(sumStepNew$steps)
```
- The **mean** and **median** of total number of steps taken per day are 10749.7704918 and 10641 respectively. We got a very small difference between previous values.


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data.


```r
dataNew['dateIs'] <- factor(sapply(dataNew$date, convert.weekday))

avgStepDateIs <- aggregate(steps~interval + dateIs, mean, data=dataNew)

xyplot( steps ~ interval | dateIs, data = avgStepDateIs, type="l", layout=c(1,2), xlab="Interval", ylab="Number of steps")
```

![](PA1_template_files/figure-html/q4-1.png) 
