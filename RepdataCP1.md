---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

  ## Introduction 
This is an R markdown document created for Course Project 1 in 
reproducible research. In this document, we aim to answer the questions given below, based on data about personal movement using a activity monitoring device. Here we go !!
  
  ## Loading and processing the data
Here, I have already unzipped the data prior to writing any code
and will use the readr library in order to access the data.

``` r
library(readr)
data_activity <- read_csv("activity.csv")
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(data_activity)
```

```
## # A tibble: 6 × 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

``` r
str(data_activity)
```

```
## spc_tbl_ [17,568 × 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```
##calling head() and str() on activity reveals the dimensions and structure of the data. Str reveals that the data is a special kind of tibble.
  
  
  ## What is mean total number of steps taken per day?
  
  
first let's remove all the NA values and check to confirm.

``` r
combed_data <- data_activity[!(is.na(data_activity$steps)), ]
sum(is.na(combed_data))
```

```
## [1] 0
```
Now, we need to create a variable that groups each day and also sums it up.

``` r
totalStepsDay <- aggregate(steps ~ date, combed_data, sum)

head(totalStepsDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Now let's make the histogram 


``` r
palettePurple <- colorRampPalette(c("lavender", "purple","violet"))
hist(totalStepsDay$steps, breaks=30, xlab="Number of Steps Taken", 
     main="Total Number of Steps Taken per Day",
     col=palettePurple(40), family="serif")
```

![](RepdataCP1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

 Now,  we can call the summarise function to calculate the mean and median
 

``` r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
totalStepsSummary <- summarise(totalStepsDay, meanOfTotalSteps=mean(totalStepsDay$steps)
                               ,
                               medianOfTotalSteps=median(totalStepsDay$steps))
print(totalStepsSummary)
```

```
##   meanOfTotalSteps medianOfTotalSteps
## 1         10766.19              10765
```
 Mean  = 10766.19 and Median= 10765.
  

 What is the average daily activity pattern?
  
To look at the average daily pattern, we can use another a time series plot. For this we must use the aggregate function to split the data into groups for each interval, and then averaged with the mean function.


``` r
meanStepsInterval <- aggregate(steps ~ interval, combed_data, mean)
head(meanStepsInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```
We will use the base plotting system to plot our time series plot.


``` r
plot(meanStepsInterval$interval, meanStepsInterval$steps, type="l"
     ,xlab="intervals of 5 mins", ylab="Avg step taken per interval", col="purple")
```

![](RepdataCP1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
 
 To answer "The 5-minute interval that, on average, contains the maximum number of steps", we will use the max function.

``` r
meanStepsInterval[which.max(meanStepsInterval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

  So the interval with the maximum number of steps is interval **835**.
  
  
  
  
  
  
  
  
  
  
  ## Imputing missing values
  we can confirm that the data contains NA values by checking

``` r
any(is.na(data_activity))
```

```
## [1] TRUE
```
  Now let's find out the if the spread of NA values is uniform throughout the data set. let's create a simple line graph to see.

``` r
na_count <- aggregate(is.na(steps) ~ date, data = data_activity, sum)

colnames(na_count) <- c("date", "na_count")


plot(na_count$date, na_count$na_count, type = "l", col = "blue",
     xlab = "Date", ylab = "Number of NA Values",
     main = "Frequency of NA Values per Day")
```

![](RepdataCP1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
  
  From the graph, we can conclude it isnt randomly missing intervals, its missing from the whole day.
  
 To fill in these gaps, i will replace each missing value with the mean value for the same interval, averaged across all days.
 

``` r
imputedData <- data_activity
for(x in 1:17568) {
    if(is.na(imputedData[x, 1])==TRUE) {
        imputedData[x, 1] <- meanStepsInterval[meanStepsInterval$interval %in% imputedData[x, 3], 2]
    }
}
head(imputedData)
```

```
## # A tibble: 6 × 3
##    steps date       interval
##    <dbl> <date>        <dbl>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```
Now that the NA values have been replaced, a histogram from the imputed data can be created.

``` r
imputedTotalStepsDay <- aggregate(steps ~ date, imputedData, sum)
head(imputedTotalStepsDay)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

``` r
palettePurple <- colorRampPalette(c("lavender", "purple","violet"))
hist(imputedTotalStepsDay$steps, breaks=60, xlab="Number of Steps Taken", 
     main="Total Number of Steps Taken per Day (With Imputed Values)",
     col=palettePurple(10), family="serif")
```

![](RepdataCP1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
Calculation of the mean and median total number of steps taken per day using the summarise function.

``` r
imputedStepsSummary <- summarise(imputedTotalStepsDay, 
                                 meanOfTotalSteps=mean(imputedTotalStepsDay$steps), 
                                 medianOfTotalSteps=median(imputedTotalStepsDay$steps))  
print(imputedStepsSummary)
```

```
##   meanOfTotalSteps medianOfTotalSteps
## 1         10766.19           10766.19
```
We can now compared the earlier result without imputed data using the rbind function.


``` r
rbind(totalStepsSummary, imputedStepsSummary)
```

```
##   meanOfTotalSteps medianOfTotalSteps
## 1         10766.19           10765.00
## 2         10766.19           10766.19
```
On comparison, we can observe that mean values are exactly the same and the median values are almost identical. This is likely due to the use of the average functions when imputing the NA measurements.

histogram comparison of the two datasets


``` r
par(mfrow = c(1, 2))

hist(totalStepsDay$steps, breaks=20, xlab="Number of Steps Taken", 
     col="blue", family="serif", ylim=c(0, 20), main=NULL)
hist(imputedTotalStepsDay$steps, breaks=20, xlab="Number of Steps Taken", 
     col="red", family="serif", ylim=c(0, 20), main=NULL)
```

![](RepdataCP1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


Frequency of values increases in the second histogram, which is expected due to the imputed values.


  
  ## Are there differences in activity patterns between weekdays and weekends?

we will create a variable with in the data frame to indicate whether a day is a weekday or weekend.


``` r
daysData <- imputedData
daysData$days <- weekdays(daysData$date)
daysData$weekday <- as.character(rep(0, times=17568))
for(x in 1:17568) {
    if(daysData[x, 4] %in% c("Saturday", "Sunday")) {
        daysData[x, 5] <- "weekend"
    } else {
        daysData[x, 5] <- "weekday"
    }
}
daysData$weekday <- factor(daysData$weekday)

weekdayData <- daysData[daysData$weekday=="weekday", ]
weekendData <- daysData[daysData$weekday=="weekend", ]
head(weekdayData)
```

```
## # A tibble: 6 × 5
##    steps date       interval days   weekday
##    <dbl> <date>        <dbl> <chr>  <fct>  
## 1 1.72   2012-10-01        0 Monday weekday
## 2 0.340  2012-10-01        5 Monday weekday
## 3 0.132  2012-10-01       10 Monday weekday
## 4 0.151  2012-10-01       15 Monday weekday
## 5 0.0755 2012-10-01       20 Monday weekday
## 6 2.09   2012-10-01       25 Monday weekday
```

``` r
head(weekendData)
```

```
## # A tibble: 6 × 5
##   steps date       interval days     weekday
##   <dbl> <date>        <dbl> <chr>    <fct>  
## 1     0 2012-10-06        0 Saturday weekend
## 2     0 2012-10-06        5 Saturday weekend
## 3     0 2012-10-06       10 Saturday weekend
## 4     0 2012-10-06       15 Saturday weekend
## 5     0 2012-10-06       20 Saturday weekend
## 6     0 2012-10-06       25 Saturday weekend
```


let's calculate the average number of steps per interval for weekday and weekend.



``` r
wdMean<-aggregate(steps~interval,weekdayData,mean)
weMean<-aggregate(steps~interval,weekendData,mean)
```

Finally let's create a panel plot.



``` r
par(mfrow=c(2, 1), mar=c(2,2,2,1))
plot(wdMean$interval, wdMean$steps, type="l",
     main="Average Steps Taken per Interval(Weekday)",
     xlab="Intervals (in 5 mins)", ylab="Number of Steps", family="serif",
     col="darkred", lwd=1.5, ylim=c(0, 230))
plot(weMean$interval, weMean$steps, type="l",
     main=" Average Steps Taken per Interval (Weekends)",
     xlab="Intervals (in 5 mins)", ylab="Number of Steps", family="serif",
     col="darkblue", lwd=1.5, ylim=c(0, 230))
```

![](RepdataCP1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->





