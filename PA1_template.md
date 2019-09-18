---
title: "Reproducible Research: Project 1"
author: "Raphael Cabrera"
date: "9/17/2019"
output: html_document
---

In this project, the goal is explore the data and generate a few plots, answering the questions along the way. 
## Part 1: Reading and Processing Data
```{r echo=TRUE}
setwd("~/Downloads/")
data <- read.csv("activity.csv")
```

## Part 2: What is mean total number of steps taken per day?

1. Calculate the total number of steps per day
* Here, the `tapply()` function was used to the sum of the steps variable on the restriction date. This was then assigned to the object `step.sums`.
```{r echo=TRUE}
step.sums <- tapply(data$steps, data$date, sum)
```

2. Make a histogram of total number of steps taken each day
* `step.sums` was used to generate a histogram.
```{r echo=TRUE}
hist(step.sums,
     xlab = "Total Steps per Day",
     main = "Histogram of Total Steps per Day",
     ylim = c(0,30),
     col = "salmon"
     )
```

3. Calculate and report the mean and median of the total number of steps taken per day
* The `median()` and `mean()` commands were used to find these respective values. `na.rm` was set to `TRUE` in order to remove `NA` values.
```{r echo=TRUE}
median(step.sums, na.rm = TRUE)
mean(step.sums, na.rm = TRUE)
```

## Part 3: What is the average daily activity pattern?

1. Make a time series plot of the average steps taken across all days
* Here, the `tapply()` function was used to find the mean of the steps variable on the restriction date. This was then assigned to the object `int.mean`. Then, a variable, `interval`, was created to plot `int.mean` against. Finally, the two objects were joined with `cbind()` to create `int.means`.
```{r echo=TRUE}
int.mean <- tapply(data$steps, data$interval, mean, na.rm = T)
interval <- unique(data$interval)
int.means <- cbind(int.mean,interval)
```
* The data is then plotted with `interval` assigned as the x axis and `int.mean` as the y axis.
```{r echo=TRUE}
plot(x=interval, y=int.mean,
     xlab = "Time Interval",
     ylab = "Daily Mean",
     main = "Average Steps per 5 minute Interval",
     type = "l",
     col = "violetred3"
     )
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
* This question is easily answered with the `max()` function, seen below:
```{r echo=TRUE}
max(int.mean)
```

## Part 4: Imputing missing values

1. Calculate and report the total number of missing values in the dataset
* This question is easily answered with the `is.na()` function, nested inside of the `sum()` function,  seen below:
```{r echo=TRUE}
sum(is.na(data$date))
```

2. Devise a strategy to impute missing data in the dataset
* Fortunately, all of the `NA` values are centralized in one feature vector: the `steps` variable. In order to impute missing data from this vector, it was decided that, to preserve the interval means as well as the date means,the only way to do that would be to replace the missing values with their respective interval means. For example, a missing value whose interval is `5`, would be replaced with the mean of all steps whose interval is equal to 5. In order to do this, first, the original dataset, `data` was copied to `data.n`. Then, a for-loop was constructed with an if statement nested inside. This loop would essentially go through each cell and determine if there was an `NA` value in this cell. If there wasn't, the cell would be skipped. If there was, then this `NA` would be replaced by the mean relating to that cell's time interval located in column 3 of `data.n`. This can be observed below:
```{r echo=TRUE}
data.n <- data
for (i in 1:length(data$steps)){
    if (is.na(data$steps[i])){
        data.n[i,1] <- subset(int.means, int.means[,2]==data.n[i,3])[,1]
    } else {
        next
    }
}
```
* After a somewhat complex method of imputing data, it is important to check if all of the `NA` values have been replaced.
```{r echo=TRUE}
#check for NAs
sum(is.na(data.n[,1]))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
* The `tapply()` function is used on the `steps` variable in `data.n` and a sum for each condition is found. A histogram is then plotted based on these values.
```{r echo=TRUE}
nsum.date <- tapply(data.n$steps, data.n$date, sum)

hist(nsum.date,
     xlab = "Day",
     main = "Histogram of New Day Sums",
     col = "wheat"
     )
```
* The mean and median of each category are found by using the `mean()` and `median()` functions respectively. These were compared with the previous, un-imputed data.
```{r echo=TRUE}
#mean of sum totals per day
mean(nsum.date)
mean(step.sums, na.rm = TRUE)

#median of sum totals per day
median(nsum.date)
median(step.sums, na.rm = TRUE)
```
* Based on these results, we can see that the means of the imputed and un-imputed data do not differ at all; However, the medians of the imputed and unimputed data differ by only -1.19.

## Part 5: Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
* In order to label each day as "weekday" or "weekend", each date must first be labeled with a respective day of the week.
```{r echo=TRUE}
day.of.week <- weekdays(as.Date(data.n[,2]))
```
* A for-loop is then generated with the goal of turning each day of the week label into a "weekday" versus "weekend" classification. The resulting vector is then bound to `data.n` using the `cbind()` function.
```{r echo=TRUE}
weekday.end <- as.character(rep(0, length(day.of.week)))
for (i in 1:length(day.of.week)){
    if (day.of.week[i]=="Saturday"){
        weekday.end[i] <- "Weekend"
    } else if (day.of.week[i]=="Sunday"){
        weekday.end[i] <- "Weekend"
    } else{
        weekday.end[i] <- "Weekday"
    }
}
data.n <- cbind(data.n, weekday.end)
```

2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days
* First, change the graphical paramaters using the functions `par()`, and `mfrow`. Then create the plot using the base plotting function `plot()`, with the following aesthetic parameters.
```{r echo=TRUE}
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
```
As a final note, it is always good practice to clean up your workspace. To do this, remove all objects and datasets that you have created and/or imported, using the `rm(list = ls())` function. 
