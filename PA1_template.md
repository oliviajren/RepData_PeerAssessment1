PA1\_template
================
Jie\_Ren
13/02/2018

**Default setting**

``` r
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo=TRUE,
  error = FALSE,
  fig.align = "center"
)

setwd("~/Desktop/5922/R")
library(ggplot2)
library(scales)
library(Hmisc)
library(gridExtra)
library(knitr)
```

Loading and preprocessing the data
----------------------------------

### 1. Reading in the dataset and/or processing the data

``` r
activity <- read.csv("activity.csv", header = T)
# Remove na rows 
activityona <- na.omit(activity)
activityona$date <- as.Date(activityona$date, format="%d/%m/%y")
```

What is mean total number of steps taken per day?
-------------------------------------------------

### 1. Make a histogram of the total number of steps taken each day

``` r
# Calculate the total number of steps taken per day
activitysum <- aggregate(activityona["steps"], by=activityona["date"], sum)

# Make a histogram of the total number of steps taken each day
as <- ggplot(data = activitysum, aes(x = date, y = steps))
as + geom_bar(stat = "identity")
```

<img src="PA1_template_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
# Method 1
plot(activitysum$date, activitysum$steps, type="h", main="Histogram of Daily Steps",xlab="Date", ylab="Steps per Day", col="black", lwd=8)
```

<img src="PA1_template_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-2.png" style="display: block; margin: auto;" />

``` r
# Method 2
ggplot(activitysum, aes(x=date, y=steps)) + 
        geom_bar(stat="identity", fill = "blue") +
        ggtitle("Histogram of Daily Steps") +
        ylab("Steps per Day") + 
        xlab("Date") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="PA1_template_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-3.png" style="display: block; margin: auto;" /> \#\#\# 2. Calculate and report the mean and median total number of steps taken per day

``` r
# Daily result
## Method 1
stepmedian<- aggregate(activityona["steps"], by=activityona["date"], median)
stepmedian
```

    ##          date steps
    ## 1  2012-10-02     0
    ## 2  2012-10-03     0
    ## 3  2012-10-04     0
    ## 4  2012-10-05     0
    ## 5  2012-10-06     0
    ## 6  2012-10-07     0
    ## 7  2012-10-09     0
    ## 8  2012-10-10     0
    ## 9  2012-10-11     0
    ## 10 2012-10-12     0
    ## 11 2012-10-13     0
    ## 12 2012-10-14     0
    ## 13 2012-10-15     0
    ## 14 2012-10-16     0
    ## 15 2012-10-17     0
    ## 16 2012-10-18     0
    ## 17 2012-10-19     0
    ## 18 2012-10-20     0
    ## 19 2012-10-21     0
    ## 20 2012-10-22     0
    ## 21 2012-10-23     0
    ## 22 2012-10-24     0
    ## 23 2012-10-25     0
    ## 24 2012-10-26     0
    ## 25 2012-10-27     0
    ## 26 2012-10-28     0
    ## 27 2012-10-29     0
    ## 28 2012-10-30     0
    ## 29 2012-10-31     0
    ## 30 2012-11-02     0
    ## 31 2012-11-03     0
    ## 32 2012-11-05     0
    ## 33 2012-11-06     0
    ## 34 2012-11-07     0
    ## 35 2012-11-08     0
    ## 36 2012-11-11     0
    ## 37 2012-11-12     0
    ## 38 2012-11-13     0
    ## 39 2012-11-15     0
    ## 40 2012-11-16     0
    ## 41 2012-11-17     0
    ## 42 2012-11-18     0
    ## 43 2012-11-19     0
    ## 44 2012-11-20     0
    ## 45 2012-11-21     0
    ## 46 2012-11-22     0
    ## 47 2012-11-23     0
    ## 48 2012-11-24     0
    ## 49 2012-11-25     0
    ## 50 2012-11-26     0
    ## 51 2012-11-27     0
    ## 52 2012-11-28     0
    ## 53 2012-11-29     0

``` r
stepmean <- aggregate(activityona["steps"], by=activityona["date"], mean)
stepmean
```

    ##          date      steps
    ## 1  2012-10-02  0.4375000
    ## 2  2012-10-03 39.4166667
    ## 3  2012-10-04 42.0694444
    ## 4  2012-10-05 46.1597222
    ## 5  2012-10-06 53.5416667
    ## 6  2012-10-07 38.2465278
    ## 7  2012-10-09 44.4826389
    ## 8  2012-10-10 34.3750000
    ## 9  2012-10-11 35.7777778
    ## 10 2012-10-12 60.3541667
    ## 11 2012-10-13 43.1458333
    ## 12 2012-10-14 52.4236111
    ## 13 2012-10-15 35.2048611
    ## 14 2012-10-16 52.3750000
    ## 15 2012-10-17 46.7083333
    ## 16 2012-10-18 34.9166667
    ## 17 2012-10-19 41.0729167
    ## 18 2012-10-20 36.0937500
    ## 19 2012-10-21 30.6284722
    ## 20 2012-10-22 46.7361111
    ## 21 2012-10-23 30.9652778
    ## 22 2012-10-24 29.0104167
    ## 23 2012-10-25  8.6527778
    ## 24 2012-10-26 23.5347222
    ## 25 2012-10-27 35.1354167
    ## 26 2012-10-28 39.7847222
    ## 27 2012-10-29 17.4236111
    ## 28 2012-10-30 34.0937500
    ## 29 2012-10-31 53.5208333
    ## 30 2012-11-02 36.8055556
    ## 31 2012-11-03 36.7048611
    ## 32 2012-11-05 36.2465278
    ## 33 2012-11-06 28.9375000
    ## 34 2012-11-07 44.7326389
    ## 35 2012-11-08 11.1770833
    ## 36 2012-11-11 43.7777778
    ## 37 2012-11-12 37.3784722
    ## 38 2012-11-13 25.4722222
    ## 39 2012-11-15  0.1423611
    ## 40 2012-11-16 18.8923611
    ## 41 2012-11-17 49.7881944
    ## 42 2012-11-18 52.4652778
    ## 43 2012-11-19 30.6979167
    ## 44 2012-11-20 15.5277778
    ## 45 2012-11-21 44.3993056
    ## 46 2012-11-22 70.9270833
    ## 47 2012-11-23 73.5902778
    ## 48 2012-11-24 50.2708333
    ## 49 2012-11-25 41.0902778
    ## 50 2012-11-26 38.7569444
    ## 51 2012-11-27 47.3819444
    ## 52 2012-11-28 35.3576389
    ## 53 2012-11-29 24.4687500

``` r
## Method 2
aggregate(steps ~ date, data = activityona, summary)
```

    ##          date steps.Min. steps.1st Qu. steps.Median steps.Mean
    ## 1  2012-10-02     0.0000        0.0000       0.0000     0.4375
    ## 2  2012-10-03     0.0000        0.0000       0.0000    39.4200
    ## 3  2012-10-04     0.0000        0.0000       0.0000    42.0700
    ## 4  2012-10-05     0.0000        0.0000       0.0000    46.1600
    ## 5  2012-10-06     0.0000        0.0000       0.0000    53.5400
    ## 6  2012-10-07     0.0000        0.0000       0.0000    38.2500
    ## 7  2012-10-09     0.0000        0.0000       0.0000    44.4800
    ## 8  2012-10-10     0.0000        0.0000       0.0000    34.3800
    ## 9  2012-10-11     0.0000        0.0000       0.0000    35.7800
    ## 10 2012-10-12     0.0000        0.0000       0.0000    60.3500
    ## 11 2012-10-13     0.0000        0.0000       0.0000    43.1500
    ## 12 2012-10-14     0.0000        0.0000       0.0000    52.4200
    ## 13 2012-10-15     0.0000        0.0000       0.0000    35.2000
    ## 14 2012-10-16     0.0000        0.0000       0.0000    52.3800
    ## 15 2012-10-17     0.0000        0.0000       0.0000    46.7100
    ## 16 2012-10-18     0.0000        0.0000       0.0000    34.9200
    ## 17 2012-10-19     0.0000        0.0000       0.0000    41.0700
    ## 18 2012-10-20     0.0000        0.0000       0.0000    36.0900
    ## 19 2012-10-21     0.0000        0.0000       0.0000    30.6300
    ## 20 2012-10-22     0.0000        0.0000       0.0000    46.7400
    ## 21 2012-10-23     0.0000        0.0000       0.0000    30.9700
    ## 22 2012-10-24     0.0000        0.0000       0.0000    29.0100
    ## 23 2012-10-25     0.0000        0.0000       0.0000     8.6530
    ## 24 2012-10-26     0.0000        0.0000       0.0000    23.5300
    ## 25 2012-10-27     0.0000        0.0000       0.0000    35.1400
    ## 26 2012-10-28     0.0000        0.0000       0.0000    39.7800
    ## 27 2012-10-29     0.0000        0.0000       0.0000    17.4200
    ## 28 2012-10-30     0.0000        0.0000       0.0000    34.0900
    ## 29 2012-10-31     0.0000        0.0000       0.0000    53.5200
    ## 30 2012-11-02     0.0000        0.0000       0.0000    36.8100
    ## 31 2012-11-03     0.0000        0.0000       0.0000    36.7000
    ## 32 2012-11-05     0.0000        0.0000       0.0000    36.2500
    ## 33 2012-11-06     0.0000        0.0000       0.0000    28.9400
    ## 34 2012-11-07     0.0000        0.0000       0.0000    44.7300
    ## 35 2012-11-08     0.0000        0.0000       0.0000    11.1800
    ## 36 2012-11-11     0.0000        0.0000       0.0000    43.7800
    ## 37 2012-11-12     0.0000        0.0000       0.0000    37.3800
    ## 38 2012-11-13     0.0000        0.0000       0.0000    25.4700
    ## 39 2012-11-15     0.0000        0.0000       0.0000     0.1424
    ## 40 2012-11-16     0.0000        0.0000       0.0000    18.8900
    ## 41 2012-11-17     0.0000        0.0000       0.0000    49.7900
    ## 42 2012-11-18     0.0000        0.0000       0.0000    52.4700
    ## 43 2012-11-19     0.0000        0.0000       0.0000    30.7000
    ## 44 2012-11-20     0.0000        0.0000       0.0000    15.5300
    ## 45 2012-11-21     0.0000        0.0000       0.0000    44.4000
    ## 46 2012-11-22     0.0000        0.0000       0.0000    70.9300
    ## 47 2012-11-23     0.0000        0.0000       0.0000    73.5900
    ## 48 2012-11-24     0.0000        0.0000       0.0000    50.2700
    ## 49 2012-11-25     0.0000        0.0000       0.0000    41.0900
    ## 50 2012-11-26     0.0000        0.0000       0.0000    38.7600
    ## 51 2012-11-27     0.0000        0.0000       0.0000    47.3800
    ## 52 2012-11-28     0.0000        0.0000       0.0000    35.3600
    ## 53 2012-11-29     0.0000        0.0000       0.0000    24.4700
    ##    steps.3rd Qu. steps.Max.
    ## 1         0.0000   117.0000
    ## 2        15.0000   613.0000
    ## 3        30.2500   547.0000
    ## 4        15.2500   555.0000
    ## 5        35.2500   526.0000
    ## 6        34.0000   523.0000
    ## 7        20.0000   748.0000
    ## 8        32.2500   413.0000
    ## 9         7.2500   748.0000
    ## 10       32.0000   802.0000
    ## 11       24.2500   542.0000
    ## 12       22.2500   540.0000
    ## 13        7.0000   786.0000
    ## 14       26.0000   758.0000
    ## 15       11.2500   744.0000
    ## 16        0.0000   759.0000
    ## 17       20.2500   512.0000
    ## 18       13.0000   532.0000
    ## 19       17.2500   501.0000
    ## 20       18.2500   783.0000
    ## 21       12.2500   499.0000
    ## 22       14.2500   533.0000
    ## 23        0.0000   443.0000
    ## 24       15.0000   440.0000
    ## 25        6.0000   555.0000
    ## 26       38.2500   533.0000
    ## 27        0.0000   591.0000
    ## 28       22.0000   523.0000
    ## 29       22.2500   757.0000
    ## 30        8.2500   753.0000
    ## 31       25.5000   533.0000
    ## 32        7.0000   785.0000
    ## 33       11.2500   630.0000
    ## 34       20.5000   766.0000
    ## 35        0.0000   359.0000
    ## 36       26.0000   540.0000
    ## 37        0.0000   542.0000
    ## 38       13.5000   444.0000
    ## 39        0.0000    33.0000
    ## 40        0.0000   475.0000
    ## 41       10.2500   753.0000
    ## 42       29.2500   785.0000
    ## 43        6.0000   789.0000
    ## 44        0.0000   500.0000
    ## 45        0.0000   758.0000
    ## 46       42.2500   567.0000
    ## 47       21.2500   760.0000
    ## 48       16.2500   785.0000
    ## 49       20.5000   551.0000
    ## 50       18.2500   709.0000
    ## 51       17.5000   806.0000
    ## 52        0.0000   733.0000
    ## 53        0.0000   568.0000

``` r
# Total result
mean(activitysum$steps)
```

    ## [1] 10766.19

``` r
median(activitysum$steps)
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

### 1. Time series plot of the average number of steps taken

``` r
activityave <- aggregate(x=list(meanSteps=activity$steps),
                                      by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
ggplot(activityave, aes(x = interval, y = meanSteps)) + 
        geom_line() + 
        ggtitle("Time series plot for average steps taken") +
        xlab("5-minute interval") +
        ylab("Average number of steps taken")
```

<img src="PA1_template_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

### 2. The 5-minute interval that, on average, contains the maximum number of steps

``` r
MostSteps <- which.max(activityave$meanSteps)
cat("The row that contaisn maximum number of steps =",MostSteps)
```

    ## The row that contaisn maximum number of steps = 104

``` r
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activityave[MostSteps,'interval'])
cat("The time that contaisn maximum number of steps =",timeMostSteps)
```

    ## The time that contaisn maximum number of steps = 8:35

Imputing missing values
-----------------------

### 1. Calculate and report the total number of missing values in the dataset

``` r
# Method 1
nano1 <- sapply(activity, function(x) sum(is.na(x)))
# Method 2
nano2 <- sum(is.na(activity$steps))
cat("Total number of missing values in the dataset",nano2)
```

    ## Total number of missing values in the dataset 2304

The total number of missing values in the dataset is 2304 rows.

### 2. filling in all of the missing values in the dataset.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

I decided to fill in all the NAs with the average value.

``` r
imputed <- activity
imputed$steps <- impute(activity$steps, fun=mean)

# Test total number of missing value
cat("Test total number of missing values =",sum(is.na(imputed$imputed_steps)))
```

    ## Test total number of missing values = 0

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

``` r
imputedsum <- aggregate(imputed["steps"], by=imputed["date"], sum)
ggplot(imputedsum, aes(x=date, y=steps)) + 
        geom_bar(stat="identity", fill = "blue") +
        ggtitle("Histogram of Daily Steps (Imputed)") +
        ylab("Steps per Day") + 
        xlab("Date") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="PA1_template_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
mean(imputedsum$steps)
```

    ## [1] 10766.19

``` r
median(imputedsum$steps)
```

    ## [1] 10766.19

**Do these values differ from the estimates from the first part of the assignment?** The mean is same as the previous part (10766.19). However, the median is now different. Previous median is 10765 and now changed to 10766.19.

**What is the impact of imputing missing data on the estimates of the total daily number of steps?** The distribution of histogram is changed.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
activityweek <- activityona
activityweek$weektime <- as.factor(ifelse(weekdays(activityona$date) %in% c("Saturday","Sunday"),"weekend", "weekday"))

Weekday <- subset(activityweek, weektime=="weekday")
Weekend <- subset(activityweek, weektime=="weekend")
```

### 2.Make a panel plot containing a time series plot (i.e. ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
# Weekday Histogram
Weekdayave <- aggregate(x=list(meanSteps=Weekday$steps),
                                      by=list(interval=Weekday$interval), FUN=mean, na.rm=TRUE)
WeekdayH <- ggplot(Weekdayave, aes(x = interval, y = meanSteps)) + 
        geom_line() + 
        ggtitle("Time series plot for average steps taken in Weekday") +
        xlab("5-minute interval") +
        ylab("Average number of steps taken")

# Weekend Histogram
Weekendave <- aggregate(x=list(meanSteps=Weekend$steps),
                                      by=list(interval=Weekend$interval), FUN=mean, na.rm=TRUE)
WeekendH <- ggplot(Weekendave, aes(x = interval, y = meanSteps)) + 
        geom_line() + 
        ggtitle("Time series plot for average steps taken in Weekend") +
        xlab("5-minute interval") +
        ylab("Average number of steps taken")

# Compare two time series together
grid.arrange(WeekdayH, WeekendH, nrow = 2)
```

<img src="PA1_template_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
# Method 2
activityweekave <- aggregate(steps ~ interval + weektime, data=activityweek, mean)
ggplot(activityweekave, aes(interval, steps)) + 
        geom_line() + 
        ggtitle("Time series plot in Weekday & Weekend") + 
        facet_grid(weektime ~ .) +
        xlab("5-minute interval") + 
        ylab("avarage number of steps")
```

<img src="PA1_template_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-2.png" style="display: block; margin: auto;" />
