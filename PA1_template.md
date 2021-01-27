---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.0.4     v dplyr   1.0.2
## v tidyr   1.1.2     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(dplyr)
library(tibble)
library(tidyr)
library(readr)

if(!file.exists('activity.csv')){
    unzip('activity.zip')
}

data <- read_csv("activity.csv")
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(data)
```

```
## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)
library(dplyr)
data2 <- data%>%
  na.omit()%>%
  group_by(date)%>%
  summarize(sum_step = sum(steps , na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
qplot(data2$sum_step, geom="histogram" ,  binwidth=500 , 
      xlab = "Daily steps" ,
      main = "Histogram of the total number of steps taken each day") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate and print mean of total steps
mean_total_steps <- mean(data2$sum_step , na.rm = TRUE)
print(mean_total_steps)
```

```
## [1] 10766.19
```

```r
# Calculate and print median of total steps
median_total_steps <- median(data2$sum_step,  na.rm = TRUE)
print(median_total_steps)
```

```
## [1] 10765
```

- Mean:  10766.19

- Median: 10765

## What is the average daily activity pattern?


```r
library(ggplot2)
library(dplyr)

data3 <- data%>%
  group_by(interval)%>%
  summarize(sum_interval = sum(steps , na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(data3 , aes(x = interval , y = sum_interval)) + 
  geom_line()+
  labs(x= "Interval" , y = "Average steps" ,  
       title = "Average daily activity pattern" )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
data3%>%
  arrange(desc(sum_interval))
```

```
## # A tibble: 288 x 2
##    interval sum_interval
##       <dbl>        <dbl>
##  1      835        10927
##  2      840        10384
##  3      850         9720
##  4      845         9517
##  5      830         9397
##  6      820         9071
##  7      855         8852
##  8      815         8349
##  9      825         8236
## 10      900         7603
## # ... with 278 more rows
```
The "835" 5-minute interval contains the maximum number of steps

## Imputing missing values

To fill in the missing values, I decided to use the mean of 5 minute interval.

```r
library(ggplot2)
library(dplyr)

#1. Number of missing values
na_values <- sum(is.na(data$steps))

#2-3. Fill in  missing values

neosteps <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dat <- data%>% 
   group_by(interval)%>%
   mutate(steps = neosteps(steps))

dat <- dat%>%
  group_by(date)%>%
  summarize(sum_step_na = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
qplot(dat$sum_step_na, geom="histogram" ,  binwidth=500 , 
      xlab = "Daily steps" ,
      main = "Histogram of the total number of steps taken each day with Na values fixed") 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Calculate and print mean of total steps with NA values fixed
mean_total_steps_na <- mean(dat$sum_step_na)
print(mean_total_steps_na)
```

```
## [1] 10766.19
```

```r
# Calculate and print median of total steps with Na values fixed 
median_total_steps_na <- dat
  median(dat$sum_step_na)
```

```
## [1] 10766.19
```


- Number of missing values: 2304
- Mean numbers of daily steps with missing values: 10766.19
- Median numbers of daily steps with missing values: 10766.19

So, the mean is the same, while median are increased a bit.

## Are there differences in activity patterns between weekdays and weekends?


```r
library(ggplot2)
library(dplyr)
days <- weekdays(data$date)
dt <- data%>%
    mutate(days = weekdays(date))%>%
    mutate(daytype =  ifelse(days == "Monday" | days == "Tuesday" | days == "Wednseday" | days == "Thursday" | days == "Friday", "Weekdays" , "Weekend"))

f_data <- dt%>%
  group_by(daytype, interval)%>%
  summarise( mean_steps = mean(steps , na.rm = TRUE)  )
```

```
## `summarise()` regrouping output by 'daytype' (override with `.groups` argument)
```

```r
ggplot(f_data , aes(x = interval , y = mean_steps , color = daytype)) +
  geom_line()+
  facet_grid(~daytype)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


