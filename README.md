## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

## Loading required package

Unzip data to obtain a csv file
```{r}

library(ggplot2)
library(plyr)
library(lattice)
```

Download Data
```{r}

data <- read.csv("activity.csv")
```

## Processing the Data
Correcting the type of date
```{r}
data$day <- weekdays(as.Date(data$date))
data$DateTime<- as.POSIXct(data$date, format="%Y-%m-%d")
```

Deleting NAs
```{r}
elimNas <- data[!is.na(data$steps),]
```

## What is the mean total number of steps taken per day?
Calculate the total number of steps taken per day
```{r}
TotalSteps <- tapply(data$steps, data$date, sum, na.rm = TRUE)
```

Create a Histogram of total steps taken per day
```{r}
hist(TotalSteps, xlab = "# of Steps", main = "Total # of Steps per Day", col="Grey")
```
![plot of HistSteps](./RepData_PeerAssessment1/figures/HistSteps.png) 


Calculate Mean and Median of Total Steps
```{r}
mean(TotalSteps, na.rm = TRUE)
median(TotalSteps, na.rm = TRUE)
```

## What is the average daily activity pattern?
Avg daily activity pattern
```{r}
AvgDaily <- ddply(elimNas, .(interval), summarize, AvgS = mean(steps))
```

Plot of average number of steps per interval
```{r}
with(AvgDaily,
     plot(interval,
          AvgS,
          type="l",
          xlab="Intervals",
          ylab="Average Daily # of Steps"))
```
![plot of AvgDailyPlot](./RepData_PeerAssessment1/figures/AvgDailyPlot.png) 

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
AvgDailyMax <- max(AvgDaily$AvgS)
AvgDaily[AvgDaily$AvgS == AvgDailyMax, 1]
```

## Missing Values
Calculate and report the total number of missing values in the dataset
```{r}
sum(is.na(data$steps))
```

Filling in all missing values in the dataset
Compute average number of steps per weekday 
```{r}
avgWeekday <- ddply(elimNas, .(interval, day), summarize, Avg = mean(steps))
```

Create dataset with all NAs for substitution
```{r}
NAs <- data[is.na(data$steps),]
```

Merge data
```{r}
mergedData <- merge(NAs, avgWeekday , by = c("interval", "day"))
```

Merge non NA data with NA data
```{r}
mergedData2 <- mergedData[,c(6,4,1,2,5)]
colnames(mergedData2)<- c("steps", "date", "interval", "day", "DateTime")

officialData <- rbind(elimNas, mergedData2)
```

Make a histogram of the total number of steps taken each day and Calculate 
and report the mean and median total number of steps taken per day

Create new sum of official data
```{r}
TotalSteps2 <- aggregate(officialData$steps ~ officialData$date, FUN=sum, )
colnames(TotalSteps2)<- c("Date", "Steps")
```

Recalculate mean and median with official data
```{r}
mean(TotalSteps2$Steps)
median(TotalSteps2$Steps)
```

Histogram
```{r}
hist(TotalSteps2$Steps, breaks=5, xlab="# of Steps", main = "Total # of Steps per Day (Imputed data)", col="Grey")
```
![plot of ImputedPlot](./RepData_PeerAssessment1/figures/ImputedPlot.png) 

## Are there differences in activity patterns between weekdays and weekends?
New category for weekday and weekend
```{r}
officialData$NewCat <- ifelse(officialData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")


mergedData3 <- ddply(officialData, .(interval, NewCat), summarize, Avg = mean(steps))
```
Plot data in a panel plot
```{r}
xyplot(Avg~interval|NewCat, data = mergedData3, layout = c(1,2), type = "l",
       main = "Actvitiy Patterns based on Weekday or Weekend", 
       ylab = "Avg # of Steps", xlab="Interval", col="Red")
```

![plot of PanelPlot](./RepData_PeerAssessment1/figures/PanelPlot.png) 
