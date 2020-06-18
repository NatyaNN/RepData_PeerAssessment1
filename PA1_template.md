---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

First, we need to clean up the R environment, load "knitr" library, and set the global options.


```r
# clean up the environment
rm(list = ls())

# setup chunk options
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warnings = FALSE,
  fig.align = "center",
  comment = "#>"
)

# Load library "knitr"
library ("knitr")
```

```
## Warning: package 'knitr' was built under R version 3.6.3
```

Next, we check the working directory (and set it, if needed) and read the csv file.


```r
getwd()
```

```
#> [1] "D:/Users/natya.n.namaskara/Documents/My Documents/Coursera R/5. Reproducible Research/RepData_PeerAssessment1-master"
```

```r
data <- read.csv("activity.csv")
```

Change the "date" variable into date format.


```r
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?

Calculate the number of steps taken per day and visualize it as histogram.  


```r
# Load libraries
library("dplyr")
```

```
#> Warning: package 'dplyr' was built under R version 3.6.3
```

```r
library("ggplot2")

# Aggrerate steps per day
sum.steps <- data %>%
  group_by(date) %>%
  summarize(steps.per.day = sum(steps))

# Make histogram
qplot(sum.steps$steps.per.day, binwidth = 1000,
      xlab = "total number of steps per day",
      ylab = "steps")
```

```
#> Warning: Removed 8 rows containing non-finite values (stat_bin).
```

<img src="PA1_template_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

Calculate and report the mean and median of the total number of steps taken per day.    


```r
mean(sum.steps$steps.per.day, na.rm = TRUE)
```

```
#> [1] 10766.19
```

```r
median(sum.steps$steps.per.day, na.rm = TRUE)
```

```
#> [1] 10765
```

The mean of total number of steps taken per day is **10766.19** steps and the median is **10765** steps per day.  

## What is the average daily activity pattern?

Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).  


```r
# Get mean steps per interval
mean.interval <- data %>%
  group_by(interval) %>%
  summarize(steps.per.interval = mean(steps, na.rm = TRUE))

# Make plot
ggplot(data = mean.interval, aes(x = interval, y = steps.per.interval)) +
  geom_line() +
  ggtitle("Average Number of Steps by Interval") +
  xlab("5-minute interval") +
  ylab("Average No. of Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
sum.interval <- data %>%
  group_by(interval) %>%
  summarize(steps = sum(steps, na.rm = TRUE))

sum.interval[which.max(sum.interval$steps),]
```

```
#> # A tibble: 1 x 2
#>   interval steps
#>      <int> <int>
#> 1      835 10927
```

Maximum average number of steps (10927 steps) happened in **835th** 5-minute interval.  

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs).  


```r
sum(is.na(data$steps))
```

```
#> [1] 2304
```

There are **2304** missing values in the dataset.  

Replace missing values with the mean of steps. This will be done in the new dataset.  


```r
# Copy new data as new dataset
data2 <- data

# Replace NAs with mean
data2$steps[is.na(data2$steps)] <- mean(data2$steps, na.rm = TRUE)
```

Here are the first rows of the new data.  


```r
head(data2)
```

```
#>     steps       date interval
#> 1 37.3826 2012-10-01        0
#> 2 37.3826 2012-10-01        5
#> 3 37.3826 2012-10-01       10
#> 4 37.3826 2012-10-01       15
#> 5 37.3826 2012-10-01       20
#> 6 37.3826 2012-10-01       25
```

We can also check the number of missing values of the new data, which is already **zero**.  


```r
sum(is.na(data2$steps))
```

```
#> [1] 0
```

Now we will develop histogram based on the **original data** (with NAs) and **completed data** (without NAs).


```r
# Load required library
library("gridExtra")
```

```
#> Warning: package 'gridExtra' was built under R version 3.6.3
```

```r
# Aggrerate steps per day for original data
sum.steps1 <- data %>%
  group_by(date) %>%
  summarize(steps.per.day = sum(steps))

# Make histogram for original data
plot1 <- qplot(sum.steps1$steps.per.day, binwidth = 1000, ylim = c(0, 15),
      xlab = "total number of steps per day",
      ylab = "steps",
      main = "original data")

# Aggrerate steps per day for completed data
sum.steps2 <- data2 %>%
  group_by(date) %>%
  summarize(steps.per.day = sum(steps))

# Make histogram for completed data
plot2 <- qplot(sum.steps2$steps.per.day, binwidth = 1000, ylim = c(0, 15),
      xlab = "total number of steps per day",
      ylab = "steps",
      main = "completed data")

# Put 2 plots in grid
grid.arrange(plot1, plot2, ncol = 2)
```

```
#> Warning: Removed 8 rows containing non-finite values (stat_bin).
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

Based on the histogram, there is an increase in the middle part of the histogram. Now we will compare the mean and median between the original data and the completed data.  


```r
# Calculate mean and median for original data
mean(sum.steps1$steps.per.day, na.rm = TRUE)
```

```
#> [1] 10766.19
```

```r
median(sum.steps1$steps.per.day, na.rm = TRUE)
```

```
#> [1] 10765
```


```r
# Calculate mean and median for completed data
mean(sum.steps2$steps.per.day, na.rm = TRUE)
```

```
#> [1] 10766.19
```

```r
median(sum.steps2$steps.per.day, na.rm = TRUE)
```

```
#> [1] 10766.19
```

The mean between the two datasets is similar. But the median of the completed data is higher than the original data.  

## Are there differences in activity patterns between weekdays and weekends?

Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  


```r
# Create new variable "day" in the completed dataset
data2$day <- weekdays(data$date)

# Categorize the days as "weekday" and "weekend" (in bahasa Indonesia)
data2$day.type <- ifelse(data2$day == "Sabtu" | data2$day == "Minggu", "weekend", "weekday")
data2$day.type <- as.factor(data2$day.type)

# Get mean steps per day type
mean.daytipe <- data2 %>%
  group_by(day.type, interval) %>%
  summarize(steps = mean(steps, na.rm = TRUE))

# Make plots and put them in grid
ggplot(data = mean.daytipe, aes(x = interval, y = steps)) +
  geom_line() +
  facet_wrap(~mean.daytipe$day.type)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

