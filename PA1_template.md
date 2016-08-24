# Reproducible Research: Peer Assessment 1





## Loading and preprocessing the data


```r
if(!file.exists("activity.csv")){
        unzip("activity.zip")
}

data <- read.csv("activity.csv")

require(dplyr)

stepsByDate <- data %>% group_by(date) %>% 
  summarise(steps = sum(steps))
```


## What is mean total number of steps taken per day?

```r
require(ggplot2)
require(ggthemes)
require(xtable)
ggplot(stepsByDate, aes(steps)) + geom_histogram(fill = '#334385', color = 'black') + theme_economist() + labs(title = 'Histogram over Steps') + theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean <- mean(stepsByDate$steps, na.rm = TRUE)
median <- median(stepsByDate$steps, na.rm = TRUE)
```
- Mean: 1.0766189\times 10^{4}
- Median: 10765


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
