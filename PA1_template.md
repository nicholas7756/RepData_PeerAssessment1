---
title: "Reproducible Research Project 1"
---

## Loading and preprocessing the data

First, load the required packages and read the activity monitoring dataset.


``` r
library(data.table)
library(ggplot2)

activityDT <- fread("activity.csv")
```

## What is the mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day


``` r
Total_Steps <- activityDT[
    ,
    .(steps = sum(steps, na.rm = FALSE)),
    by = date
]

head(Total_Steps, 30)
```

```
##           date steps
##         <IDat> <int>
##  1: 2012-10-01    NA
##  2: 2012-10-02   126
##  3: 2012-10-03 11352
##  4: 2012-10-04 12116
##  5: 2012-10-05 13294
##  6: 2012-10-06 15420
##  7: 2012-10-07 11015
##  8: 2012-10-08    NA
##  9: 2012-10-09 12811
## 10: 2012-10-10  9900
## 11: 2012-10-11 10304
## 12: 2012-10-12 17382
## 13: 2012-10-13 12426
## 14: 2012-10-14 15098
## 15: 2012-10-15 10139
## 16: 2012-10-16 15084
## 17: 2012-10-17 13452
## 18: 2012-10-18 10056
## 19: 2012-10-19 11829
## 20: 2012-10-20 10395
## 21: 2012-10-21  8821
## 22: 2012-10-22 13460
## 23: 2012-10-23  8918
## 24: 2012-10-24  8355
## 25: 2012-10-25  2492
## 26: 2012-10-26  6778
## 27: 2012-10-27 10119
## 28: 2012-10-28 11458
## 29: 2012-10-29  5018
## 30: 2012-10-30  9819
##           date steps
```

### 2. Histogram of the total number of steps taken each day


``` r
ggplot(Total_Steps[!is.na(steps)], aes(x = steps)) +
    geom_histogram(binwidth = 1000, fill = "grey", color = "black") +
    labs(
        title = "Total Number of Steps Taken per Day",
        x = "Total Steps per Day",
        y = "Frequency"
    )
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### 3. Mean and median total number of steps taken per day


``` r
Total_Steps[
    ,
    .(
        Mean_Steps = mean(steps, na.rm = TRUE),
        Median_Steps = median(steps, na.rm = TRUE)
    )
]
```

```
##    Mean_Steps Median_Steps
##         <num>        <int>
## 1:   10766.19        10765
```

## What is the average daily activity pattern?

### 1. Average number of steps for each 5-minute interval

The average number of steps is calculated for each 5-minute interval across all days.


``` r
IntervalDT <- activityDT[
    ,
    .(steps = mean(steps, na.rm = TRUE)),
    by = interval
]
```

The average daily activity pattern is shown below.


``` r
ggplot(IntervalDT, aes(x = interval, y = steps)) +
    geom_line(color = "green", linewidth = 1) +
    labs(
        title = "Average Daily Activity Pattern",
        x = "5-Minute Interval",
        y = "Average Number of Steps"
    )
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

### 2. Five-minute interval with the maximum average number of steps


``` r
IntervalDT[
    steps == max(steps),
    .(Maximum_Interval = interval, Average_Steps = steps)
]
```

```
##    Maximum_Interval Average_Steps
##               <int>         <num>
## 1:              835      206.1698
```

## Imputing missing values

The number of missing observations is first calculated, followed by an imputation procedure.

### 1. Total number of missing values


``` r
sum(is.na(activityDT$steps))
```

```
## [1] 2304
```

### 2. Strategy for imputing missing values

Missing step values are replaced with the mean number of steps for the corresponding 5-minute interval, calculated across all available days.

This strategy preserves the average daily activity pattern. For example, a missing value during an early-morning interval is replaced with the average activity observed during that same interval on other days rather than with a single overall value for the entire dataset.

### 3. Create a new dataset with missing values filled in

A copy of the original dataset is created so that the original data remain unchanged.


``` r
imputedDT <- copy(activityDT)

intervalMeans <- activityDT[
    ,
    .(interval_mean = mean(steps, na.rm = TRUE)),
    by = interval
]

imputedDT <- merge(
    imputedDT,
    intervalMeans,
    by = "interval",
    all.x = TRUE,
    sort = FALSE
)

imputedDT[
    is.na(steps),
    steps := interval_mean
]
```

```
## Warning in `[.data.table`(imputedDT, is.na(steps), `:=`(steps, interval_mean)): 1.716981 (type 'double') at RHS
## position 1 out-of-range(NA) or truncated (precision lost) when assigning to type 'integer' (column 2 named
## 'steps')
```

``` r
imputedDT[, interval_mean := NULL]
```


### 4. Daily steps after imputing missing values

Calculate the total number of steps taken per day using the imputed dataset.


``` r
Total_Steps_Imputed <- imputedDT[
    ,
    .(steps = sum(steps)),
    by = date
]
```

Create a histogram of total daily steps after imputation.


``` r
ggplot(Total_Steps_Imputed, aes(x = steps)) +
    geom_histogram(binwidth = 1000, fill = "grey", color = "black") +
    labs(
        title = "Total Number of Steps per Day After Imputation",
        x = "Total Steps per Day",
        y = "Frequency"
    )
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

Calculate the mean and median total number of steps taken per day after imputation.


``` r
Total_Steps_Imputed[
    ,
    .(
        Mean_Steps = mean(steps),
        Median_Steps = median(steps)
    )
]
```

```
##    Mean_Steps Median_Steps
##         <num>        <int>
## 1:   10749.77        10641
```

For comparison, the estimates before imputation are:


``` r
Total_Steps[
    ,
    .(
        Mean_Steps = mean(steps, na.rm = TRUE),
        Median_Steps = median(steps, na.rm = TRUE)
    )
]
```

```
##    Mean_Steps Median_Steps
##         <num>        <int>
## 1:   10766.19        10765
```

The estimates after imputation may differ slightly from those obtained when missing days were excluded. Imputing missing observations using the average value for each 5-minute interval adds estimated activity for previously incomplete days while maintaining the overall daily activity pattern. This can change both the distribution of total daily steps and the corresponding mean and median.

## Are there differences in activity patterns between weekdays and weekends?

The imputed dataset is used for this part of the analysis.

### 1. Create a weekday/weekend factor variable

First, convert the date variable to a date format and classify each observation as either a weekday or weekend.


``` r
imputedDT[, date := as.Date(date)]

imputedDT[, daytype :=
    ifelse(
        as.POSIXlt(date)$wday %in% c(0, 6),
        "weekend",
        "weekday"
    )
]

imputedDT[, daytype :=
    factor(daytype, levels = c("weekday", "weekend"))
]

table(imputedDT$daytype)
```

```
## 
## weekday weekend 
##   12960    4608
```

### 2. Compare weekday and weekend activity patterns

Calculate the average number of steps for each 5-minute interval separately for weekdays and weekends.


``` r
IntervalWeekDT <- imputedDT[
    ,
    .(steps = mean(steps)),
    by = .(interval, daytype)
]
```

Create a panel plot comparing weekday and weekend activity patterns.


``` r
ggplot(IntervalWeekDT, aes(x = interval, y = steps)) +
    geom_line(color = "grey", linewidth = 1) +
    facet_wrap(~daytype, ncol = 1) +
    labs(
        title = "Average Activity Pattern: Weekdays vs Weekends",
        x = "5-Minute Interval",
        y = "Average Number of Steps"
    )
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

The two panels allow the average activity pattern during weekdays to be compared directly with the pattern observed during weekends.
