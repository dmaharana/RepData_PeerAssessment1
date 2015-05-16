rm(list = ls())
activity_df <- read.csv('data/repdata-data-activity/activity.csv', stringsAsFactors=FALSE);
str(activity_df);
activity_df$date <- as.Date(activity_df$date, "%Y-%m-%d")

# What is mean total number of steps taken per day?
# Make a histogram of the total number of steps taken each day
library(data.table)

# calculate steps taken per day
activity_dt <- na.omit(data.table(activity_df))
stepsByDate <- data.frame(activity_dt[, sum(steps), by = date])
names(stepsByDate) <- c('date', 'steps') 

# plot steps taken per day
library(ggplot2)
ggplot(stepsByDate, aes(x=steps)) +
  geom_histogram(colour="black", fill="white", binwidth = 1000) +
  labs(x = 'Steps', y = 'Frequency', title = 'Total Steps by Day')

# Calculate and report the mean and median total number of steps taken per day
# mean of steps taken per day
mean(stepsByDate$steps)

# meadian of steps taken per day
median(stepsByDate$steps)

# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avg_steps <- data.frame(activity_dt[, mean(steps), by = interval])
names(avg_steps) <- c('interval', 'steps')
ggplot(data = avg_steps) +
  geom_line(aes(x = interval, y = steps, colour = "Average Steps"))+
  labs(x = 'Interval', y = 'Steps', title = 'Average Number of Steps taken across all days')

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval_pos <- avg_steps$steps == max(avg_steps$steps)
max_interval <- avg_steps[max_interval_pos, 1]

# Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

NA_Count <- sum(is.na(activity_df$steps))

# Devise a strategy for filling in all of the missing values in the dataset.
# Fill the NAs with median

StepsMedian <- aggregate(steps ~ interval, data = activity_df, FUN = median)

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
new_activity_df <- data.frame(activity_df)
na_pos <- which(is.na(activity_df$steps))

for (idx in na_pos){
  new_activity_df[idx, 'steps'] <- StepsMedian[which(activity_df[5, 'interval'] == StepsMedian$interval), 'steps']
}

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
new_stepsByDate <- aggregate(new_activity_df$steps, by = list(new_activity_df$date), FUN = sum)
names(new_stepsByDate) <- c('date', 'steps')

ggplot(new_stepsByDate, aes(x=steps)) +
  geom_histogram(colour="black", fill="white", binwidth = 1000) +
  labs(x = 'Steps', y = 'Frequency', title = 'Total Steps by Day with NAs filled with Median of respective interval')

mean(new_stepsByDate$steps)
median(new_stepsByDate$steps)

# Are there differences in activity patterns between weekdays and weekends?

new_activity_df <- cbind(new_activity_df, weekday = tolower(weekdays(new_activity_df$date)))
new_activity_df <- cbind(new_activity_df, dayType = ifelse(new_activity_df$weekday == 'saturday'|
                                                             new_activity_df$weekday == 'sunday', 'weekend',
                                                           'weekday'))
avg_steps <- aggregate(new_activity_df$steps, by = list(new_activity_df$interval, new_activity_df$dayType), FUN = mean)
names(avg_steps) <- c('interval', 'dayType', 'meanSteps')
# Compute the time serie plot

# Load the lattice graphical library
library(lattice)
xyplot(meanSteps ~ interval | dayType, avg_steps, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))