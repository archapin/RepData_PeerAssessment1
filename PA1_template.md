# Reproducible Research: Peer Assessment 1




## Downloading the data

The data can be downloaded from [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

This code gets the data downloaded and stored in a csv file name activity.csv in the working directory.


```r
if(!file.exists("activity.csv")){
        zipfileURL = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(zipfileURL, destfile="activity.zip", method = "libcurl")
        unzip("activity.zip")
}
```


## Loading and preprocessing the data

The data in the *activity.csv* file is load into R. Then we take a fist look at the data, using the head, tail and str functions of r


```r
activityData = read.csv("activity.csv")
head(activityData,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
tail(activityData,10)
```

```
##       steps       date interval
## 17559    NA 2012-11-30     2310
## 17560    NA 2012-11-30     2315
## 17561    NA 2012-11-30     2320
## 17562    NA 2012-11-30     2325
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activityData)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
 
 
It can be seen that:
- The *steps* variable has 2304 missing values denoted by NA, and recognized as integer type 
- The *date* variable is recognized as a factor so it needs to be transformed into a Date type
- The *interval* variable is recognized as an integer type with no NA's
 
 Getting the *date* variable converted in date format.
 

```r
activityData$date = as.Date(activityData$date) 
summary(activityData$date)
```

```
##         Min.      1st Qu.       Median         Mean      3rd Qu. 
## "2012-10-01" "2012-10-16" "2012-10-31" "2012-10-31" "2012-11-15" 
##         Max. 
## "2012-11-30"
```

```r
str(activityData$date)
```

```
##  Date[1:17568], format: "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
```
 
The variable *date* is coerced into the Date type and checked for missing values.  It runs from Oct 1, 2012 to Nov 15, 2012.

## What is mean total number of steps taken per day?

Firts, we take a look at a histogram or the total number of steps taken per day.
Second, we see a summary of the daily total number of steps statistics 


```r
stepsPerDay = data.frame(stepsPerDay = with(activityData, tapply(steps, date, function(x) sum(x, na.rm= TRUE))))
library(ggplot2)
g1 <- ggplot(data = stepsPerDay, aes(stepsPerDay))
g1 + geom_histogram()+labs(x="Total number of steps in a day", y = "Frequency", title = "Histogram")
```

![](PA1_template_files/figure-html/question 1-1.png) 

```r
library(xtable)

summSteps <- summary(stepsPerDay$stepsPerDay)
xt <- xtable(t(summSteps))
caption(xt) = "Statistics for Daily Total Number of Steps"
print(xt, type = "html",include.rownames = FALSE)
```

<!-- html table generated in R 3.2.0 by xtable 1.7-4 package -->
<!-- Wed Jun 10 13:50:18 2015 -->
<table border=1>
<caption align="bottom"> Statistics for Daily Total Number of Steps </caption>
<tr> <th> Min. </th> <th> 1st Qu. </th> <th> Median </th> <th> Mean </th> <th> 3rd Qu. </th> <th> Max. </th>  </tr>
  <tr> <td align="right"> 0.00 </td> <td align="right"> 6778.00 </td> <td align="right"> 10400.00 </td> <td align="right"> 9354.00 </td> <td align="right"> 12810.00 </td> <td align="right"> 21190.00 </td> </tr>
   </table>

The median is 10400 daily total steps and the mean is 9354 daily total steps. 



## What is the average daily activity pattern?


```r
meanStepsInterval = data.frame(mean = with(activityData, 
                                           tapply(steps, interval, 
                                                  function(x) mean(x, na.rm = TRUE))))

plot(rownames(meanStepsInterval),meanStepsInterval$mean,type="l", main="Average Number of Daily Steps per 5-minute Interval", xlab = "5-minute Interval", ylab="" )
```

![](PA1_template_files/figure-html/question 2-1.png) 


```r
# Interval with the maximum number of steps

intervalMaxSteps = as.integer(names(which.max(meanStepsInterval$mean)))
maxAverSteps = as.numeric(meanStepsInterval$mean[which.max(meanStepsInterval$mean)])
intervalPrev = intervalMaxSteps - 5

intervalHours1 = as.integer(intervalMaxSteps/60)
intervalMinutes1 = intervalMaxSteps - intervalHours1*60

intervalHours2 = as.integer(intervalPrev/60)
intervalMinutes2 = intervalPrev - intervalHours2*60
```

The maximum average number of steps is 206.1698113 and it occurs between  13 : 50 and 13 : 55.

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
