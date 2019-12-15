---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Firstly we need to check if the file exists. If not - we unpack it in our working directory. 

```r
filename<- "activity.zip"
if (!file.exists("activity.csv")) { 
  unzip(filename) 
}
data<-read.csv("activity.csv")
```

Convert dates in the corresponding format and add a column for weekdays (we will need it to answer question 8).

```r
data$date <- as.POSIXct(data$date, "%Y-%m-%d")
data$weekday <- weekdays(data$date)
head(data)
```

```
##   steps       date interval   weekday
## 1    NA 2012-10-01        0 понеділок
## 2    NA 2012-10-01        5 понеділок
## 3    NA 2012-10-01       10 понеділок
## 4    NA 2012-10-01       15 понеділок
## 5    NA 2012-10-01       20 понеділок
## 6    NA 2012-10-01       25 понеділок
```

## What is mean total number of steps taken per day?
Firstly we need to aggregate the data by day.

```r
data.steps<-aggregate(data$steps,list(data$date),sum,na.rm=TRUE)
names(data.steps)<-c("date","steps")
head(data.steps)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```
## png 
##   2
```

```r
hist(data.steps$steps, main="Total steps taken per day", xlab = "Steps", col="green",ylim=c(0,10), breaks = seq(0,25000, by=1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
  
  Now we are ready to find the mean and the median of the total number of steps, taken by day:

```r
mean(data.steps$steps,na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(data.steps$steps,na.rm=TRUE)
```

```
## [1] 10395
```
## What is the average daily activity pattern?
Now we have to aggregate the number of steps over every interval:

```
## png 
##   2
```


```r
plot(data.steps.interval$interval, data.steps.interval$steps, type = "l", lwd = 2, xlab="Interval", ylab="Steps", main="Average number of steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The interval with the maximal mean value of steps is

```r
data.steps.interval[data.steps.interval$mean==max(data.steps.interval$steps,na.rm=TRUE),][[1]]
```

```
## integer(0)
```

## Imputing missing values
Let us find the columns with the missing values:

```r
sum(is.na(data$date))
```

```
## [1] 0
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
sum(is.na(data$interval))
```

```
## [1] 0
```
  
The only column with the missing values is the "steps" column. We can substitute the missing values by the rounded mean values steps.

```r
m <- round(mean(data.steps.interval$steps,na.rm=TRUE))
na.index<-is.na(data[,1])
complete.data<-data
complete.data[na.index,1]<-m
sum(is.na(complete.data$steps))
```

```
## [1] 0
```

Let us reproduce the steps from thesecond section for the new data set.

Firstly we need to aggregate the data by day.

```r
complete.data.steps<-aggregate(complete.data$steps,list(complete.data$date),sum,na.rm=TRUE)
names(complete.data.steps)<-c("date","steps")
head(complete.data.steps)
```

```
##         date steps
## 1 2012-10-01 10656
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
mean(complete.data.steps$steps)
```

```
## [1] 10751.74
```

```r
median(complete.data.steps$steps)
```

```
## [1] 10656
```

```
## png 
##   2
```

```r
hist(complete.data.steps$steps, main="Total steps taken per day (new data)", xlab = "Steps", col="green",ylim=c(0,20), breaks = seq(0,25000, by=1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
  
  Now we are ready to find the mean and the median of the total number of steps, taken by day:

```r
mean(complete.data.steps$steps)
```

```
## [1] 10751.74
```

```r
median(complete.data.steps$steps)
```

```
## [1] 10656
```
  
As we can see, the output has changed, the estimates of the total daily number of steps increased.



## Are there differences in activity patterns between weekdays and weekends?
We have to add a column, which would indicate whether it's weekday or not. Then, using "ggplot2" library, we print the plot.


```r
library(ggplot2)
data$Day <- sapply(data$weekday, function(x) {
        if (x == "субота" | x =="неділя") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
data.day <- aggregate(steps~interval + Day, data, mean, na.rm = TRUE)
plot<- ggplot(data.day, aes(x = interval , y = steps, color = Day)) +
       geom_line() +
       labs(title = "Average steps by day", x = "Interval", y = "Steps") +
       facet_wrap(~Day, ncol = 1, nrow=2) 
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

