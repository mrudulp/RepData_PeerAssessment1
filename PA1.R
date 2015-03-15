require(plyr)
require(dplyr)

# Read csv file
activity <- read.csv("activity.csv")
#activity[is.na(activity)] <- 0

# - What is mean total number of steps taken per day?

# # Part one
##aggregate
# * Calculate the total number of steps taken per day
agg <- aggregate(steps~date,data=activity,sum)
# *histogram of the total number of steps taken each day
hist(agg$steps,plot=TRUE, main="Histogram of Steps per Day")
# *mean and median of the total number of steps taken per day
mean(agg$steps) #10766.19
# median
median(agg$steps) #10765
    
# - What is the average daily activity pattern?
agg <- aggregate(steps~interval,data=activity,mean)
    # *Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(agg$interval, agg$steps, type = "l", xlab = "Interval", ylab = "Average number of steps")

    # *5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps?
agg[agg$steps==max(agg$steps),]$interval #835

# - Imputing missing values

#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

 #   Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps)) #2304

  #  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    
  #  Create a new dataset that is equal to the original dataset but with the missing data filled in.
  	activity2 <- activity
    activity2[is.na(activity2)] <- 0 # replacing NAs with Zero

  #  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  	agg <- aggregate(steps~date,data=activity,mean)
  	hist(agg$steps,plot=TRUE, main="Histogram of Steps per Day")
  	mean(agg$steps) #37.3826
  	median(agg$steps) #37.37847

# Are there differences in activity patterns between weekdays and weekends?

# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity2 <- mutate(activity2,wdays=weekdays(as.Date(date),abbreviate=T))
activity2 <- mutate(activity2,weekday=wdays %in% c("Mon","Tue","Wed","Thu","Fri")) #find weekdays
activity2 <- mutate(activity2,weekend=wdays %in% c("Sun","Sat"))
weekDayActivity <- activity2[activity2$weekday==TRUE,]
weekendActivity <- activity2[activity2$weekend==TRUE,]

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
aggWeekDay <- aggregate(steps~interval,data=weekDayActivity,mean)
aggWeekend <- aggregate(steps~interval,data=weekendActivity,mean)

par(mfrow=c(2,1))
plot(aggWeekDay$interval, aggWeekDay$steps, type = "l", xlab = "Interval", ylab = "Average number of steps",col="blue",main="WeekDay")
plot(aggWeekend$interval, aggWeekend$steps, type = "l", xlab = "Interval", ylab = "Average number of steps", col="green",main="WeekEnd")
