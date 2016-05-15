# Reproducible Research Assignment 1

## Introduction
This is part of Coursera Reproducible Research course assignment #1. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and Tidying Data
The raw data was given in a zip file through a link on the assignment page. I downloaded and unzipped the data in as "activity.csv".

Load the data.

```r
data <- read.csv("activity.csv", header = TRUE)
```

Check the data.

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Change the data format from factor to date format using the lubridate package installed from CRAN.

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.4
```

```
## 
## Attaching package: 'lubridate'
## 
## The following object is masked from 'package:base':
## 
##     date
```

```r
data$date <- ymd(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Now data is ready to be analysed.

## What is the mean total number of steps taken per day?
1. Aggregate the number of steps into the total number of steps per day
2. Make a histogram of the total number of steps to see the trend
3. Calculate the mean

### Analysis
1. Aggregate the number of steps into the total number of steps per day using dplyr and group by date

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
steps <- data %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (date) (int)
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
## ..        ...   ...
```

2. Make a histogram using ggplot2

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
ggplot(steps, aes(x = steps)) + 
    geom_histogram(fill = "red", binwidth = 1000) +
    labs(title = "Histogram of Total Steps per Day", x = "Steps per Day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

3. Calculate the mean and median steps per day

```r
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
```

Mean steps per day is 10,766, and median steps per day is 10,765.

## What is the average daily activity pattern?
1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

### Analysis
1. Calculate the average number of steps taken in each 5-minute interval across all days, using dplyr

```r
average_interval <- data %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(steps = mean(steps))
```

Make a time series plot of the 5-minute interval and the average number of steps taken, using ggplot2

```r
ggplot(average_interval, aes(x = interval, y = steps)) +
    geom_line(color = "red") +
    labs(title = "Time Series Plot of Average Number os Steps", x = "5-minute Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Find which interval contains the maximum number of steps on average across all days

```r
average_interval[which.max(average_interval$steps), ]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```
The interval 835 contains the maximum number of steps, 206 steps. 

## Inputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰.s)
2. Devise a strategy for filling in all of the missing values in the dataset.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

### Analysis
1. Calculate the total number of missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
There are 2,304 missing values.

2. Let's fill in a missing value with the average number of steps taken in the same interval.
3. Create a new dataset equal to the original but with the missing data filled in.

```r
data_filled <- data
na <- is.na(data_filled$steps)
avg_interval <- tapply(data_filled$steps, data_filled$interval, mean, na.rm = TRUE, simplify = TRUE)
data_filled$steps[na] <- avg_interval[as.character(data_filled$interval[na])]
```

Check to see if there are no missing values.

```r
sum(is.na(data_filled$steps))
```

```
## [1] 0
```
Good - no missing values any more.

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 

```r
steps_filled <- data_filled %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
##        (date)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```


```r
ggplot(steps_filled, aes(x = steps)) + 
    geom_histogram(fill = "red", binwidth = 1000) +
    labs(title = "Histogram of Total Steps per Day (with imputed missing values)", x = "Steps per Day", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

Calculate the mean and median total number of steps.

```r
mean_steps_filled <- mean(steps_filled$steps)
median_steps_filled <- median(steps_filled$steps)
mean_steps_filled
```

```
## [1] 10766.19
```

```r
median_steps_filled
```

```
## [1] 10766.19
```
The impact of imputing missing values with the average number of steps in the same interval is that the mean number of steps and the median number of steps are the same value, 10,766.19.

## Are there differences in activity patterns between weekdays and weekends?
For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

### Analysis
1. Add a new column, day_type, that indicates a given day is weekday or weekend, bu using dplyr and mutate.


```r
data_filled <- mutate(data_filled, day_type = ifelse(weekdays(data_filled$date) == "Saturday" | weekdays(data_filled$date) == "Sunday", "weekend", "weekday"))
data_filled$day_type <- as.factor(data_filled$day_type)
head(data_filled)
```

```
##       steps       date interval day_type
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

2. Aggregate the average number os total steps per interval per day_type (weekday or weekend). Make a panel plot showing a time series plot of the intervals for weekdays, and another time series plot for weekends.

Aggregate:

```r
interval_filled <- data_filled %>%
    group_by(interval, day_type) %>%
    summarise(steps = mean(steps))
```

Plot:

```r
panel_plot <- ggplot(interval_filled, aes(x = interval, y = steps, color = day_type)) +
    geom_line() +
    facet_wrap(~ day_type, ncol = 1, nrow = 2)
print(panel_plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png) 

The panel plot suggests that 
- The test subject becomes active earlier in the day on weekdays than on weekends. 
- There is a sudden hike in the number of steps taken in some 5-minute intervals on the weekdays. The highest number of steps taken in such intervals is over 100 steps higher on weekdays than the higest number of steps on weekends.
- On the other hand, the subject is more active throughout the day on weekends than on weekdays. 
