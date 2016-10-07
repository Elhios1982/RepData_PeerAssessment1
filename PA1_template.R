rm(list = ls())
library(dplyr)
library(lubridate)
library(stringr)
setwd("C:/coursera/data-science/RepData_PeerAssessment1")
## Loading and preprocessing the data
df <- tbl_df(read.table(unz("activity.zip", "activity.csv"), header = T, quote="\"", sep = ",", na.strings = "NA", colClasses = "character"))
df <- df %>%
      mutate(steps = as.integer(steps)) %>%
      mutate(date = ymd(date)) %>%
      mutate(interval = str_pad(df$interval, 4, pad = "0"))

## Histogram of total number of steps taken each day
df.complete.cases <- df[complete.cases(df),]
df.total.steps <- aggregate(steps ~ date, data = df.complete.cases, FUN = sum)
hist(df.total.steps$steps)

## Mean and Median total number of steps per day
print(summarize(group_by(df.complete.cases, date), Mean = mean(steps), Median = median(steps) ), n=53)

## Plot average steps taken per day interval
df.mean.by.interval <- summarize( group_by(df.complete.cases, interval), Mean = mean(steps) )
plot(df.mean.by.interval$interval, df.mean.by.interval$Mean, type = "l", xlab = "Interval", main = "Daily Activity Pattern", ylab = "Average number of steps" )

## Max number of steps per day interval
max(df.mean.by.interval$Mean)

# Imputing missing values
## calculate and report the total number of missing values in the dataset
sum( complete.cases(df) == "FALSE" )


## Devise a strategy for filling missing values
df.mean.by.interval










