# Reproducible Research: Peer Assessment 1





## Loading and preprocessing the data


```r
require(dplyr, quietly = TRUE)


if(!file.exists("activity.csv")){
        unzip("activity.zip")
}

data <- read.csv("activity.csv")



stepsByDate <- data %>% group_by(date) %>% 
        summarise(steps = sum(steps, na.rm = TRUE))

avgStepsByInterval <- data %>% group_by(interval) %>% summarise(avgSteps = mean(steps, na.rm = TRUE))
```


## What is mean total number of steps taken per day?

```r
require(ggplot2)
require(ggthemes)
ggplot(stepsByDate, aes(steps)) + geom_histogram(fill = '#334385', color = 'black', binwidth = 500) + theme_economist() + labs(title = 'Histogram over Steps') + theme(plot.title = element_text(hjust = 0.5))
```

![](figure/unnamed-chunk-3-1.png)<!-- -->

```r
mean <- mean(stepsByDate$steps, na.rm = TRUE)
median <- median(stepsByDate$steps, na.rm = TRUE)
```
- Mean: 9354.2295082
- Median: 10395


## What is the average daily activity pattern?

```r
ggplot(avgStepsByInterval, aes(interval, avgSteps)) + geom_line(color ='#33877D') + theme_economist() + ylab('Average Steps')
```

![](figure/unnamed-chunk-4-1.png)<!-- -->

```r
maxInterval <- avgStepsByInterval$interval[which.max(avgStepsByInterval$avgSteps)]
```
- Interval which has the maximum of average steps: 835


## Imputing missing values

```r
numNA <- sum(is.na(data$steps))
```

- Number of NA's: 2304


```r
## The strategy for replacing missing values is to impute avg of steps by respective interval into where missing value is

dataInput <- data

replaceNA <- function(var, interval){
        if(is.na(var)){
                var <- avgStepsByInterval$avgSteps[avgStepsByInterval$interval == interval]
        }
        else
                var
}

dataInput$steps <- mapply(replaceNA, dataInput$steps, dataInput$interval)


stepsByDateInput <- dataInput %>% group_by(date) %>% 
        summarise(steps = sum(steps, na.rm = TRUE))

ggplot(stepsByDateInput, aes(steps)) + geom_histogram(fill = '#334385', color = 'black', binwidth = 500) + theme_economist() + labs(title = 'Histogram over Steps (after imputation)') + theme(plot.title = element_text(hjust = 0.5))
```

![](figure/unnamed-chunk-6-1.png)<!-- -->

```r
meanInput <- mean(stepsByDateInput$steps, na.rm = TRUE)
medianInput <- median(stepsByDateInput$steps, na.rm = TRUE)
```
- Mean: 1.0766189\times 10^{4}
- Median: 1.0766189\times 10^{4}



## Are there differences in activity patterns between weekdays and weekends?


```r
dataInput$weekpart <- ifelse(as.POSIXlt(dataInput$date)$wday %in% c(6,0), 'weekend', 'weekday') %>% as.factor()

avgStepsByIntervalInput <- dataInput %>% group_by(interval, weekpart) %>% summarise(avgSteps = mean(steps, na.rm = TRUE))

ggplot(avgStepsByIntervalInput, aes(interval, avgSteps)) + geom_line(color ='#33877D') + theme_economist() + facet_wrap(~weekpart, nrow = 2) + ylab('Average Steps')
```

![](figure/unnamed-chunk-7-1.png)<!-- -->
