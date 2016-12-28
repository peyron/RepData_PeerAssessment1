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

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)

mean <- mean(stepsByDate$steps, na.rm = TRUE)
median <- median(stepsByDate$steps, na.rm = TRUE)
mean2 <- mean(total.steps, na.rm = TRUE)
median2 <- median(total.steps, na.rm = TRUE)
```
- Mean: 9354.2295082
- Median: 10395
- Mean: 9354.2295082
- Median: 10395


## What is the average daily activity pattern?

```r
ggplot(avgStepsByInterval, aes(interval, avgSteps)) + geom_line(color ='#33877D') + theme_economist() + ylab('Average Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

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
dataInput <- data
#[i]

replaceNA <- function(var, interval){
        if(is.na(var)){
                var <- avgStepsByInterval$avgSteps[avgStepsByInterval$interval == interval]
                        #avgStepsByInterval$avgSteps[dataInput$interval == avgStepsByInterval$interval]
        }
        else
                var
}

dataInput$steps <- mapply(replaceNA, dataInput$steps, dataInput$interval)

fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}

for(i in 1:nrow(dataInput)){
        if(is.na(dataInput$steps[i])){
                dataInput$steps[i] <- avgStepsByInterval$avgSteps[dataInput$interval[i] == avgStepsByInterval$interval]
        }
}


stepsByDateInput <- dataInput %>% group_by(date) %>% 
        summarise(steps = sum(steps))

ggplot(stepsByDateInput, aes(steps)) + geom_histogram(fill = '#334385', color = 'black', binwidth = 500) + theme_economist() + labs(title = 'Histogram over Steps (after imputation)') + theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

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

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
