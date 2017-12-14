# Exercise Trends Data Analysis
Daniel Truong  

## Synopsis

This report was originally submitted as part of an assignment on **Reproducible Research**, hosted by John Hopkins University on the Coursera platform (https://www.coursera.org/learn/reproducible-research). The purpose of this report is to demonstrate the techniques that are used in cleaning up data and processing the data into a human-readable format. We will be using exercise data collected from an anonymous individual and performing simple data analyses on it (mean, median, visualization). The process of performing these analyses will be done in R and outlined thoughout the document. Eventually, we will come to find that (based on the data), physical exercises are performed much earlier on weekdays, as opposed to them being performed on the weekends. 

## Introduction

Per John Hopkins University's Reproducible Research course, devices such as the Fitbit, Nike Fuelband and the Jawbone Up have paved the way for creating the _quantified self_ movement: a movement to incorporate technology for acquiring data on a person's daily life (https://en.wikipedia.org/wiki/Quantified_Self). In the case of the aforementioned devices, fitness data is collected from an individual. Variables such as steps taken per day and heart rate are recorded and used (in a comprehensive manner) to express statistics about an individuals's fitness level and allow them to make choices on how to modify or sustain their physical exercise patterns. This report aims to analyze the steps taken in a 5-minute interval per day via pedometer function of a popular fitness device. 

## Data overview

The data (that will be used in the analyses) is hosted by John Hopkins University on Coursera. The data is a set of recorded amount of steps taken (via a pedometer function) per five-minute interval (per day). There are three variables: **steps** (the amount of steps taken within a five-minute interval), **date** (the day of when the steps were recorded) and **interval** (the five-minute interval for which the steps were recorded in). 17,568 observations were recorded in the initial analysis. 

## Loading and preprocessing the data

For this portion of the assignment, pre-planning of the data and the analysis environment will occur. The package **ggplot2** will be used for the purposes of plotting data with. The csv file, **activity.csv** will be read into the **activity** variable. The **activity** variable contains 3 variables (Steps, Date, Interval) and 17,568 samples. 


```r
# Load in dependent libraries
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.1
```

```r
# Read in activity.csv
activity <- read.csv("activity.csv")

# Rename variables in activity
names(activity) <- c("Steps", "Date", "Interval")
```

## What is mean total number of steps taken per day?

For this part, we want to find out the total number (sum) of steps taken per day, as well as the mean and median. In addition, a histogram will be constructed of the sum of steps taken per day.

Analyzing the **activity** data, we see that there's multiple instances of missing steps on certain days. For this part, we will simply ignore the days that have the missing steps. We'll store it in a separate data set labeled **activity.omitna**.


```r
activity.omitna <- na.omit(activity)
```

Below is the code and results for finding the total number (sum) of steps taken per day:


```r
steps.sum <- aggregate(activity.omitna$Steps, 
                     by = list(Day = activity.omitna$Date), 
                     FUN = sum)

names(steps.sum) <- c("Date", "Steps")

# Total number of steps taken per day
steps.sum
```

```
##          Date Steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

Below is a histogram of the total number of steps taken per day


```r
ggplot(steps.sum, aes(Steps)) + geom_histogram() + ggtitle("Histogram of steps taken per day") + xlab("Sum of steps (per day)") + ylab("Days")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Here, we have the calculations of the mean and median of the total number of steps taken per day:


```r
# Mean
steps.mean <- mean(steps.sum$Steps)
steps.mean
```

```
## [1] 10766.19
```

```r
# Median
steps.median <- median(steps.sum$Steps)
steps.median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

We'll start analyzing the average daily activity pattern by plotting the average number of steps per 5-minute interval (averaged across all days). 


```r
# Average of steps over 5-minute interval
avg.steps <- aggregate(activity$Steps, 
                      by = list(activity$Interval), 
                      FUN = mean, 
                      na.rm = TRUE)
names(avg.steps) <- c("Interval", "Steps")

# Time-series plot of average 5-minute steps
ggplot(avg.steps, aes(Interval, Steps)) + geom_line() + xlab("5-minute Interval") + ylab("Average Steps") + ggtitle("Average activity pattern\nover 5-minute intervals in a day")
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Of all averages (over a 5-minute interval), we will find the maximum amount of steps taken and the interval for which this occurs. Below is the code and results:


```r
avg.steps[which.max(avg.steps$Steps), ]
```

```
##     Interval    Steps
## 104      835 206.1698
```

## Imputing missing values

In the previous sections, values that were recorded as NA were just ignored. Here, we must deal with those missing values head-on. First, let's see how many missing values there are: 


```r
sum(is.na(activity$Steps))
```

```
## [1] 2304
```

According to the output of the code above, we have 2,304 missing values for the steps variable. To address the missing values, we'll take the average of the 5-minute intervals and impute them into the missing values:


```r
activity.imputed <- activity

# Iterate from 1 to length of activity.imputed$Steps, change NAs to average of steps per 5-minute interval
for(i in 1:length(activity.imputed$Steps)){
  if(is.na(activity.imputed$Steps[i])){
    activity.imputed$Steps[i] <- avg.steps$Steps[avg.steps$Interval == activity.imputed$Interval[i]]
  }
}
```

Now that the missing data has been imputed, let's see a histogram of the total number of steps taken per day:


```r
steps.sum.imputed <- aggregate(activity.imputed$Steps, 
                     by = list(Day = activity.imputed$Date), 
                     FUN = sum, 
                     na.rm = TRUE)

# Rename variables in steps.sum.imputed
names(steps.sum.imputed) <- c("Day", "Steps")

# Histogram of data
ggplot(steps.sum.imputed, aes(Steps)) + geom_histogram() + ggtitle("Histogram of steps taken per day (after data imputation)") + xlab("Sum of steps (per day)") + ylab("Days")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](README_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

...as well the mean and median (total number of steps taken per day)...


```r
# Mean
steps.mean.imputed <- mean(steps.sum.imputed$Steps)
steps.mean.imputed
```

```
## [1] 10766.19
```

```r
# Median
steps.median.imputed <- median(steps.sum.imputed$Steps)
steps.median.imputed
```

```
## [1] 10766.19
```

We notice that only the median has changed, but only slightly, than if the NA data was simply ignored. Below, we'll see the differences calculated between the imputed data and the omitted data:


```r
# Mean difference
steps.mean.imputed - steps.mean
```

```
## [1] 0
```

```r
# Median difference
steps.median.imputed - steps.median
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?

We will now check to see if there are major differences of steps between the weekdays and weekends. To help with this, we'll create a new factor level that will determine if a day is a weekday or weekend. Below is the code that will determine if the day is a weekday/weekend and impute that in a new feature:


```r
# Create weekday/weekend factor variable
wdCheck <- function(x) {
  # Checks if a date is on a weekend or not.
  #
  # Args:
  #   x: The date in question.
  #
  # Returns:
  #   "Weekend" if the date is on a Saturday or Sunday, Weekday if else.
  if(x == "Saturday" || x == "Sunday") 
    return ("Weekend") 
  else return ("Weekday") 
}
activity.imputed["DayType"] <- sapply(weekdays(as.Date(activity.imputed$Date)), wdCheck)
```

The code below will now compare the panel plots of steps taken per interval, separated by weekday/weekend: 


```r
activity.weektype.mean <- aggregate(Steps ~ Interval + DayType, data = activity.imputed, mean)
ggplot(activity.weektype.mean, aes(Interval, Steps)) + geom_line() + facet_grid(DayType ~ .) + xlab("Interval") + ylab("Steps") + ggtitle("Time-series plot of\nWeekday vs. Weekend Exercise")
```

![](README_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Based on a cursory glance of the plot, we see similar exercise trends (in that they occur heavily after 5:00 and peters out near 20:00). However, weekday exercise trends are stronger (morning-time exercise is greater and workout starts and ends slightly earlier). 

## Conclusion

Based on the above analysis of the exercise data, we can conclude that (in the case of the person being recorded) physical movement is more prevalent in the morning times and somewhat tapers off as the day goes on. Obviously, as the day reaches nightime, the amount of steps taken are kept to a minimum. On days that are recorded as weekends, physical movement starts slightly later in the morning and ends slightly later into the night. 
