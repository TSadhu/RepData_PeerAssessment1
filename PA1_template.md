Reproducible Research: Peer Assessment 1
==========================================


```r
echo = TRUE
```

## Loading and preprocessing the data


```r
data <- read.csv("activity.csv",colClasses = c("integer", "Date", "factor"))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
data$month <- as.numeric(format(data$date, "%m"))
data <- na.omit(data)
```

## What is mean total number of steps taken per day?

* Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
ggplot(data, aes(date, steps))+ geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + labs(title = "Histogram of Steps Taken on Each Day", x = "Date", y = "Total steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
* Calculate and report the mean and median total number of steps taken per day

Mean and Median of total number of steps taken per day:


```r
totalSteps <- aggregate(data$steps, list(Date = data$date), FUN = "sum")$x
mean(totalSteps)
```

```
## [1] 10766.19
```

```r
median(totalSteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps<-aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
```

```
## Error in aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), : object 'noNA' not found
```

```r
names(avgSteps)[2]<-"meanOfSteps"
ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835    206.1698
```

## Imputing missing values

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
sum(is.na(read.csv("activity.csv")))
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
newData <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}
```

```
## Error in nrow(newData): object 'newData' not found
```

```r
head(newData)
```

```
## Error in head(newData): object 'newData' not found
```

```r
sum(is.na(newData))
```

```
## Error in eval(expr, envir, enclos): object 'newData' not found
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(newData, aes(date, steps)) + geom_bar(stat = "identity", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

```
## Error in ggplot(newData, aes(date, steps)): object 'newData' not found
```
* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
```

```
## Error in aggregate(newData$steps, list(Date = newData$date), FUN = "sum"): object 'newData' not found
```

```r
newMean <- mean(newTotalSteps)
```

```
## Error in mean(newTotalSteps): object 'newTotalSteps' not found
```

```r
newMean
```

```
## Error in eval(expr, envir, enclos): object 'newMean' not found
```
Median total number of steps taken per day:

```r
newMedian <- median(newTotalSteps)
```

```
## Error in median(newTotalSteps): object 'newTotalSteps' not found
```

```r
newMedian
```

```
## Error in eval(expr, envir, enclos): object 'newMedian' not found
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
```

```
## Error in eval(expr, envir, enclos): object 'newMean' not found
```

```r
newMedian - oldMedian
```

```
## Error in eval(expr, envir, enclos): object 'newMedian' not found
```

## Are there differences in activity patterns between weekdays and weekends?


```r
head(newData)
```

```
## Error in head(newData): object 'newData' not found
```

```r
newData$weekdays <- factor(format(newData$date, "%A"))
```

```
## Error in format(newData$date, "%A"): object 'newData' not found
```

```r
levels(newData$weekdays)
```

```
## Error in levels(newData$weekdays): object 'newData' not found
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
```

```
## Error in levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday", : object 'newData' not found
```

```r
levels(newData$weekdays)
```

```
## Error in levels(newData$weekdays): object 'newData' not found
```

```r
table(newData$weekdays)
```

```
## Error in table(newData$weekdays): object 'newData' not found
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
```

```
## Error in aggregate(newData$steps, list(interval = as.numeric(as.character(newData$interval)), : object 'newData' not found
```

```r
names(avgSteps)[3] <- "meanOfSteps"
```

```
## Error in names(avgSteps)[3] <- "meanOfSteps": 'names' attribute [3] must be the same length as the vector [2]
```

```r
library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

```
## Error in limits.and.aspect(default.prepanel, prepanel = prepanel, have.xlim = have.xlim, : need at least one panel
```
