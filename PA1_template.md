# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity_df <- read.csv('repdata-data-activity/activity.csv', stringsAsFactors=FALSE);
str(activity_df);
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity_df$date <- as.Date(activity_df$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
### Make a histogram of the total number of steps taken each day

```r
library(data.table)
```
### Calculate steps taken per day

```r
activity_dt <- na.omit(data.table(activity_df))
stepsByDate <- data.frame(activity_dt[, sum(steps), by = date])
names(stepsByDate) <- c('date', 'steps') 
```
### Plot steps taken per day

```r
library(ggplot2)
ggplot(stepsByDate, aes(x=steps)) +
  geom_histogram(colour="black", fill="white", binwidth = 1000) +
  labs(x = 'Steps', y = 'Frequency', title = 'Total Steps by Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

### Calculate and report the mean and median total number of steps taken per day
### mean of steps taken per day

```r
mean(stepsByDate$steps)
```

```
## [1] 10766.19
```
### meadian of steps taken per day

```r
median(stepsByDate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_steps <- data.frame(activity_dt[, mean(steps), by = interval])
names(avg_steps) <- c('interval', 'steps')
ggplot(data = avg_steps) +
  geom_line(aes(x = interval, y = steps, colour = "Average Steps"))+
  labs(x = 'Interval', y = 'Steps', title = 'Average Number of Steps taken across all days')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval_pos <- avg_steps$steps == max(avg_steps$steps)
avg_steps[max_interval_pos, 1]
```

```
## [1] 835
```


## Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NA_Count <- sum(is.na(activity_df$steps))
```
### Devise a strategy for filling in all of the missing values in the dataset.
### Fill the NAs with median

```r
StepsMedian <- aggregate(steps ~ interval, data = activity_df, FUN = median)
```
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_activity_df <- data.frame(activity_df)
na_pos <- which(is.na(activity_df$steps))

for (idx in na_pos){
  new_activity_df[idx, 'steps'] <- StepsMedian[which(activity_df[5, 'interval'] == StepsMedian$interval), 'steps']
}
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
new_stepsByDate <- aggregate(new_activity_df$steps, by = list(new_activity_df$date), FUN = sum)
names(new_stepsByDate) <- c('date', 'steps')

ggplot(new_stepsByDate, aes(x=steps)) +
  geom_histogram(colour="black", fill="white", binwidth = 1000) +
  labs(x = 'Steps', y = 'Frequency', title = 'Total Steps by Day with NAs filled with Median of respective interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

#### Mean and Median after replacing NAs with Median

```r
mean(new_stepsByDate$steps)
```

```
## [1] 9354.23
```

```r
median(new_stepsByDate$steps)
```

```
## [1] 10395
```
#### Mean and Median before replacing NAs

```r
mean(stepsByDate$steps)
```

```
## [1] 10766.19
```

```r
mean(stepsByDate$steps)
```

```
## [1] 10766.19
```
### "After replacing the NAs with median both the mean and median are different"

## Are there differences in activity patterns between weekdays and weekends?
### Generate the weekday and weekend information from the date

```r
new_activity_df <- cbind(new_activity_df, weekday = tolower(weekdays(new_activity_df$date)))
new_activity_df <- cbind(new_activity_df, dayType = ifelse(new_activity_df$weekday == 'saturday'|
                                                             new_activity_df$weekday == 'sunday', 'weekend',
                                                           'weekday'))
avg_steps <- aggregate(new_activity_df$steps, by = list(new_activity_df$interval, new_activity_df$dayType), FUN = mean)
names(avg_steps) <- c('interval', 'dayType', 'meanSteps')
```
### Compute the time series plot


#### Load the lattice graphical library

```r
library(lattice)
xyplot(meanSteps ~ interval | dayType, avg_steps, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
