**Default setting**

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

Loading and preprocessing the data
----------------------------------

### 1. Reading in the dataset and/or processing the data

    activity <- read.csv("activity.csv", header = T)
    # Remove na rows 
    activityona <- na.omit(activity)
    activityona$date <- as.Date(activityona$date, format="%d/%m/%y")

What is mean total number of steps taken per day?
-------------------------------------------------

### 1. Make a histogram of the total number of steps taken each day

    # Calculate the total number of steps taken per day
    activitysum <- aggregate(activityona["steps"], by=activityona["date"], sum)

    # Make a histogram of the total number of steps taken each day
    as <- ggplot(data = activitysum, aes(x = date, y = steps))
    as + geom_bar(stat = "identity")

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

    # Method 1
    plot(activitysum$date, activitysum$steps, type="h", main="Histogram of Daily Steps",xlab="Date", ylab="Steps per Day", col="black", lwd=8)

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-2-2.png" style="display: block; margin: auto;" />

    # Method 2
    ggplot(activitysum, aes(x=date, y=steps)) + 
            geom_bar(stat="identity", fill = "blue") +
            ggtitle("Histogram of Daily Steps") +
            ylab("Steps per Day") + 
            xlab("Date") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-2-3.png" style="display: block; margin: auto;" />
\#\#\# 2. Calculate and report the mean and median total number of steps
taken per day

    # Daily result
    ## Method 1
    stepmedian<- aggregate(activityona["steps"], by=activityona["date"], median)
    stepmean <- aggregate(activityona["steps"], by=activityona["date"], mean)
    ## Method 2
    stepsummary <-aggregate(steps ~ date, data = activityona, summary)

    # Total result
    mean(activitysum$steps)

    ## [1] 10766.19

    median(activitysum$steps)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

### 1. Time series plot of the average number of steps taken

    activityave <- aggregate(x=list(meanSteps=activity$steps),
                                          by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
    ggplot(activityave, aes(x = interval, y = meanSteps)) + 
            geom_line() + 
            ggtitle("Time series plot for average steps taken") +
            xlab("5-minute interval") +
            ylab("Average number of steps taken")

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

### 2. The 5-minute interval that, on average, contains the maximum number of steps

    MostSteps <- which.max(activityave$meanSteps)
    cat("The row that contaisn maximum number of steps =",MostSteps)

    ## The row that contaisn maximum number of steps = 104

    timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activityave[MostSteps,'interval'])
    cat("The time that contaisn maximum number of steps =",timeMostSteps)

    ## The time that contaisn maximum number of steps = 8:35

Imputing missing values
-----------------------

### 1. Calculate and report the total number of missing values in the dataset

    # Method 1
    nano1 <- sapply(activity, function(x) sum(is.na(x)))
    # Method 2
    nano2 <- sum(is.na(activity$steps))
    cat("Total number of missing values in the dataset",nano2)

    ## Total number of missing values in the dataset 2304

The total number of missing values in the dataset is 2304 rows.

### 2. filling in all of the missing values in the dataset.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

I decided to fill in all the NAs with the average value.

    imputed <- activity
    imputed$steps <- impute(activity$steps, fun=mean)

    # Test total number of missing value
    cat("Test total number of missing values =",sum(is.na(imputed$imputed_steps)))

    ## Test total number of missing values = 0

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

    imputedsum <- aggregate(imputed["steps"], by=imputed["date"], sum)
    ggplot(imputedsum, aes(x=date, y=steps)) + 
            geom_bar(stat="identity", fill = "blue") +
            ggtitle("Histogram of Daily Steps (Imputed)") +
            ylab("Steps per Day") + 
            xlab("Date") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

    mean(imputedsum$steps)

    ## [1] 10766.19

    median(imputedsum$steps)

    ## [1] 10766.19

**Do these values differ from the estimates from the first part of the
assignment?** The mean is same as the previous part (10766.19). However,
the median is now different. Previous median is 10765 and now changed to
10766.19.

**What is the impact of imputing missing data on the estimates of the
total daily number of steps?** The distribution of histogram is changed.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    activityweek <- activityona
    activityweek$weektime <- as.factor(ifelse(weekdays(activityona$date) %in% c("Saturday","Sunday"),"weekend", "weekday"))

    Weekday <- subset(activityweek, weektime=="weekday")
    Weekend <- subset(activityweek, weektime=="weekend")

### 2.Make a panel plot containing a time series plot (i.e. ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

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

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

    # Method 2
    activityweekave <- aggregate(steps ~ interval + weektime, data=activityweek, mean)
    ggplot(activityweekave, aes(interval, steps)) + 
            geom_line() + 
            ggtitle("Time series plot in Weekday & Weekend") + 
            facet_grid(weektime ~ .) +
            xlab("5-minute interval") + 
            ylab("avarage number of steps")

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-10-2.png" style="display: block; margin: auto;" />
