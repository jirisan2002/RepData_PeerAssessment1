

```r
opts_chunk$set(echo = TRUE)
## loading the necessary R packages
library(plyr)
library(ggplot2)
library(xtable)
```

Analyses of Daily Walking Activity of an Anonymous Individual
========================================================
### Study Summary

- Use of small portable devices to collect data on personal activites
- An individual's daily activity was monitored by counting the number of steps per every 5-minute interval per day

### Goals of the Study

1.  To manipulate this dataset to extract, summarize and present the statiscal summaries that are appropriate to our study questions.
2.  To integrate the manipulation processes and the resulting summaries in an R markdown file to generate a document that complies lierate statistcal programming.   


### 1.  Mean Total Number of Steps per Day

- Import the data and name it "walk" 
- Add a new variable "weekday" (weekdays or weekend) based on the date. 
 
**Generation of histogram and summary table**

- **histogram** showing the distribution of total daily steps
- **table** summarizing the daily walking activities


```r
# read the data frame 'activity.csv' in the working directory
walk <- read.csv("activity.csv")

# add new varibale 'weekday' and append
weekday <- weekdays(as.Date(walk$date, format = "%m/%d/%Y"))
walk <- cbind(walk, weekday)
walk$date <- as.character(walk$date)
walk$weekday <- as.character(walk$weekday)

sortweekdays <- function(x) {
    x <- replace(x, !(x %in% c("Saturday", "Sunday")), "Weekdays")
    x <- replace(x, x %in% c("Saturday", "Sunday"), "Weekend")
}
walk$weekday <- sortweekdays(walk$weekday)

summaryTable <- ddply(walk, c("date"), summarize, total = sum(steps, na.rm = T), 
    average = round(sum(steps, na.rm = T)/length(steps)), median = median(steps, 
        na.rm = T))

g <- ggplot(summaryTable, aes(total)) + geom_histogram(binwidth = 2000, fill = "steelblue", 
    col = "red")
g <- g + ggtitle("Histogram of Total Steps per Day") + xlab("Total Steps") + 
    ylab("Frequency")
print(g)
```

![plot of chunk histogram](figure/histogram.png) 



```r
print(summaryTable, type = "html")
```

```
##          date total average median
## 1  2012-10-01     0       0     NA
## 2  2012-10-02   126       0      0
## 3  2012-10-03 11352      39      0
## 4  2012-10-04 12116      42      0
## 5  2012-10-05 13294      46      0
## 6  2012-10-06 15420      54      0
## 7  2012-10-07 11015      38      0
## 8  2012-10-08     0       0     NA
## 9  2012-10-09 12811      44      0
## 10 2012-10-10  9900      34      0
## 11 2012-10-11 10304      36      0
## 12 2012-10-12 17382      60      0
## 13 2012-10-13 12426      43      0
## 14 2012-10-14 15098      52      0
## 15 2012-10-15 10139      35      0
## 16 2012-10-16 15084      52      0
## 17 2012-10-17 13452      47      0
## 18 2012-10-18 10056      35      0
## 19 2012-10-19 11829      41      0
## 20 2012-10-20 10395      36      0
## 21 2012-10-21  8821      31      0
## 22 2012-10-22 13460      47      0
## 23 2012-10-23  8918      31      0
## 24 2012-10-24  8355      29      0
## 25 2012-10-25  2492       9      0
## 26 2012-10-26  6778      24      0
## 27 2012-10-27 10119      35      0
## 28 2012-10-28 11458      40      0
## 29 2012-10-29  5018      17      0
## 30 2012-10-30  9819      34      0
## 31 2012-10-31 15414      54      0
## 32 2012-11-01     0       0     NA
## 33 2012-11-02 10600      37      0
## 34 2012-11-03 10571      37      0
## 35 2012-11-04     0       0     NA
## 36 2012-11-05 10439      36      0
## 37 2012-11-06  8334      29      0
## 38 2012-11-07 12883      45      0
## 39 2012-11-08  3219      11      0
## 40 2012-11-09     0       0     NA
## 41 2012-11-10     0       0     NA
## 42 2012-11-11 12608      44      0
## 43 2012-11-12 10765      37      0
## 44 2012-11-13  7336      25      0
## 45 2012-11-14     0       0     NA
## 46 2012-11-15    41       0      0
## 47 2012-11-16  5441      19      0
## 48 2012-11-17 14339      50      0
## 49 2012-11-18 15110      52      0
## 50 2012-11-19  8841      31      0
## 51 2012-11-20  4472      16      0
## 52 2012-11-21 12787      44      0
## 53 2012-11-22 20427      71      0
## 54 2012-11-23 21194      74      0
## 55 2012-11-24 14478      50      0
## 56 2012-11-25 11834      41      0
## 57 2012-11-26 11162      39      0
## 58 2012-11-27 13646      47      0
## 59 2012-11-28 10183      35      0
## 60 2012-11-29  7047      24      0
## 61 2012-11-30     0       0     NA
```


### 2.  Pattern of Average Walkign Activity 

- Does this individual's walking activity show any kind of pattern?  
1.  Split the "activity (walk)" dataset by the "interval" variable
2.  Calculate the average steps for each 5-minute intervals

- **Time-series plot** showing the average steps per intervals 


```r
## new data frame 'stepPerInterval' splitted by 'interval'
stepsPerInterval <- ddply(walk, .(interval), summarize, average = mean(steps, 
    na.rm = T))
g <- ggplot(stepsPerInterval, aes(interval, average)) + geom_line()
g <- g + ggtitle("Avergae Steps per Interval") + xlab("Interval") + ylab("Average Steps")

maxStep <- max(stepsPerInterval$average, na.rm = T)
maxInterval <- stepsPerInterval$interval[stepsPerInterval$average == maxStep]
g <- g + geom_text(aes(maxInterval, maxStep), label = maxInterval, color = "red")
print(g)
```

![plot of chunk timeseries](figure/timeseries.png) 


#### This time series plot shows that this invidual's maximum walking activity is at the **835th interval**.  

### 3.  Missing Data Imputation 

```r
naCount <- sum(is.na(walk$steps))
```

- The "activity" data set has total **2304** missing values in the "steps" variable.  
- The missing values were discrded in the above analyses.  
- This process resulted in a very unlikely outcome 
(For example 8 days with zero ealking activity)

**Imputation method**
- Split by "weekday"
- Split by "interval"
- Calculate the means per interval
- Replace NAs with the means of the corresponding intervals 

**I used ddply() function of the *plyr* package to split the data.  The replacena() function is introduced to replace na's**


```r
replacena <- function(x) {
    replace(x, is.na(x), round(mean(x, na.rm = T)))
}

imputedwalk <- ddply(walk, c("weekday", "interval"), transform, steps = replacena(steps))
imputedwalk <- imputedwalk[order(as.Date(imputedwalk$date), imputedwalk$interval), 
    ]

summaryTable1 <- ddply(imputedwalk, c("date"), summarize, total = sum(steps, 
    na.rm = T), average = round(mean(steps, na.rm = T)), median = median(steps, 
    na.rm = T))
```

- The resulting **histogram** and **Summary Table** are shown below.  
- The number of days with < 2,000 steps went down from 10 to 2 days   
- A noticeable change in the summary rable is in the median value.


```r
h <- ggplot(summaryTable1, aes(total)) + geom_histogram(binwidth = 2000, fill = "steelblue", 
    col = "red")
h <- h + ggtitle("Histogram of Total Steps per Day (Imputed)") + xlab("Total Steps") + 
    ylab("Frequency")
print(h)
```

![plot of chunk histogram2](figure/histogram2.png) 


```r
print(summaryTable1, type = "html")
```

```
##          date total average median
## 1  2012-10-01 10762      37   34.5
## 2  2012-10-02   126       0    0.0
## 3  2012-10-03 11352      39    0.0
## 4  2012-10-04 12116      42    0.0
## 5  2012-10-05 13294      46    0.0
## 6  2012-10-06 15420      54    0.0
## 7  2012-10-07 11015      38    0.0
## 8  2012-10-08 10762      37   34.5
## 9  2012-10-09 12811      44    0.0
## 10 2012-10-10  9900      34    0.0
## 11 2012-10-11 10304      36    0.0
## 12 2012-10-12 17382      60    0.0
## 13 2012-10-13 12426      43    0.0
## 14 2012-10-14 15098      52    0.0
## 15 2012-10-15 10139      35    0.0
## 16 2012-10-16 15084      52    0.0
## 17 2012-10-17 13452      47    0.0
## 18 2012-10-18 10056      35    0.0
## 19 2012-10-19 11829      41    0.0
## 20 2012-10-20 10395      36    0.0
## 21 2012-10-21  8821      31    0.0
## 22 2012-10-22 13460      47    0.0
## 23 2012-10-23  8918      31    0.0
## 24 2012-10-24  8355      29    0.0
## 25 2012-10-25  2492       9    0.0
## 26 2012-10-26  6778      24    0.0
## 27 2012-10-27 10119      35    0.0
## 28 2012-10-28 11458      40    0.0
## 29 2012-10-29  5018      17    0.0
## 30 2012-10-30  9819      34    0.0
## 31 2012-10-31 15414      54    0.0
## 32 2012-11-01 10762      37   34.5
## 33 2012-11-02 10600      37    0.0
## 34 2012-11-03 10571      37    0.0
## 35 2012-11-04 10762      37   34.5
## 36 2012-11-05 10439      36    0.0
## 37 2012-11-06  8334      29    0.0
## 38 2012-11-07 12883      45    0.0
## 39 2012-11-08  3219      11    0.0
## 40 2012-11-09 10762      37   34.5
## 41 2012-11-10 10762      37   34.5
## 42 2012-11-11 12608      44    0.0
## 43 2012-11-12 10765      37    0.0
## 44 2012-11-13  7336      25    0.0
## 45 2012-11-14 10762      37   34.5
## 46 2012-11-15    41       0    0.0
## 47 2012-11-16  5441      19    0.0
## 48 2012-11-17 14339      50    0.0
## 49 2012-11-18 15110      52    0.0
## 50 2012-11-19  8841      31    0.0
## 51 2012-11-20  4472      16    0.0
## 52 2012-11-21 12787      44    0.0
## 53 2012-11-22 20427      71    0.0
## 54 2012-11-23 21194      74    0.0
## 55 2012-11-24 14478      50    0.0
## 56 2012-11-25 11834      41    0.0
## 57 2012-11-26 11162      39    0.0
## 58 2012-11-27 13646      47    0.0
## 59 2012-11-28 10183      35    0.0
## 60 2012-11-29  7047      24    0.0
## 61 2012-11-30 10762      37   34.5
```


### 4.  Comparison of Activity Patterns between Weekdays and Weekends


```r
## new data frame stepPerInterval2 splitted by 'weekday' & 'interval'
stepsPerInterval2 <- ddply(imputedwalk, .(weekday, interval), summarize, average = mean(steps, 
    na.rm = T))
f <- ggplot(stepsPerInterval2, aes(interval, average)) + geom_line()
f <- f + facet_grid(weekday ~ .)
f <- f + ggtitle("Avergae Steps per Interval (imputed)") + xlab("Interval") + 
    ylab("Average Steps")
print(f)
```

![plot of chunk timeseries2](figure/timeseries2.png) 

