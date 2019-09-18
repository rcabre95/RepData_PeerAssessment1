# pre clean- up
rm(list = ls())

# read in data
# setwd("~/Downloads/")
# data <- read.csv("activity.csv")
data <- read.csv(file.choose(), header = TRUE)

#total number of steps taken per day
step.sums <- tapply(data$steps, data$date, sum)

#histogram of daily sums
hist(step.sums,
     xlab = "Total Steps per Day",
     main = "Histogram of Total Steps per Day",
     ylim = c(0,30),
     col = "salmon"
     )


#median and mean steps per day
median(step.sums, na.rm = TRUE)
mean(step.sums, na.rm = TRUE)


#interval data
int.mean <- tapply(data$steps, data$interval, mean, na.rm = T)
interval <- unique(data$interval)
int.means <- cbind(int.mean,interval)

#time-series plot of mean intervals 
plot(x=interval, y=int.mean,
     xlab = "Time Interval",
     ylab = "Daily Mean",
     main = "Average Steps per 5 minute Interval",
     type = "l",
     col = "violetred3"
     )

#maximum average time interval
max(int.mean)

# Calculate number of NA values
sum(is.na(data$date))

#new data with NA values imputed
data.n <- data

# imputing strategy:
# replace all missing NA values with the mean value belonging to
# that time interval
for (i in 1:length(data$steps)){
    if (is.na(data$steps[i])){
        data.n[i,1] <- subset(int.means, int.means[,2]==data.n[i,3])[,1]
    } else {
        next
    }
}

#check for NAs
sum(is.na(data.n[,1]))
#none (woohoo!)

#Generate histogram of new sums by date
nsum.date <- tapply(data.n$steps, data.n$date, sum)

hist(nsum.date,
     xlab = "Day",
     main = "Histogram of New Day Sums",
     col = "wheat"
     )

#mean of sum totals per day
mean(nsum.date)
mean(step.sums, na.rm = TRUE)

#median of sum totals per day
median(nsum.date)
median(step.sums, na.rm = TRUE)

# mean does not differ. median differs by -1.19

# Create weekday/weekend variable
## Initialize day of week names
day.of.week <- weekdays(as.Date(data.n[,2]))

## Initialize data frame where day of week labels will go
weekday.end <- as.character(rep(0, length(day.of.week)))

## insert labels
for (i in 1:length(day.of.week)){
    if (day.of.week[i]=="Saturday"){
        weekday.end[i] <- "Weekend"
    } else if (day.of.week[i]=="Sunday"){
        weekday.end[i] <- "Weekend"
    } else{
        weekday.end[i] <- "Weekday"
    }
}

# update imputed table
data.n <- cbind(data.n, weekday.end)

#subset weekdays and weekends
week.days <- subset(data.n, data.n$weekday.end == "Weekday")
week.ends <- subset(data.n, data.n$weekday.end == "Weekend")

#Generate means for time intervals for weekdays/weekends
day.int.mean <- tapply(week.days$steps, week.days$interval, mean, na.rm = T)
end.int.mean <- tapply(week.ends$steps, week.ends$interval, mean, na.rm = T)

#plot lines
par(mfrow=c(2,1))
plot(x=interval, y=day.int.mean,
     xlab = "Time",
     ylab = "Daily Mean (Weekday)",
     main = "Average Steps per 5 minute Interval",
     type = "l",
     col = "orange2"
)
plot(x=interval, y=end.int.mean,
     xlab = "Time",
     ylab = "Daily Mean (Weekend)",
     col = "steelblue2",
     type = "l"
      )

# If you would like to export/save the images generated in this
# script, you can simply highlight the commented code below and
# press "CTRL+SHIFT+C" to uncomment it. Then you can simply run
# the code! :)

# png("dailysums_hist.png")
# hist(step.sums,
#      xlab = "Total Steps per Day",
#      main = "Histogram of Total Steps per Day",
#      ylim = c(0,30),
#      col = "salmon"
# )
# dev.off()
# 
# png("timeplot_all.png")
# plot(x=interval, y=int.mean,
#      xlab = "Time Interval",
#      ylab = "Daily Mean",
#      main = "Average Steps per 5 minute Interval",
#      type = "l",
#      col = "violetred3"
# )
# dev.off()
# 
# png("daysums_new.png")
# hist(nsum.date,
#      xlab = "Day",
#      main = "Histogram of New Day Sums",
#      col = "wheat"
# )
# dev.off()
# 
# png("weekday.weekend.timeplots.png")
# par(mfrow=c(2,1))
# plot(x=interval, y=day.int.mean,
#      xlab = "Time",
#      ylab = "Daily Mean (Weekday)",
#      main = "Average Steps per 5 minute Interval",
#      type = "l",
#      col = "orange2"
# )
# plot(x=interval, y=end.int.mean,
#      xlab = "Time",
#      ylab = "Daily Mean (Weekend)",
#      col = "steelblue2",
#      type = "l"
# )
# dev.off()
# 

# post clean-up
rm(list = ls())