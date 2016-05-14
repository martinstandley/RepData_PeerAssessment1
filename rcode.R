
library(dplyr)
library(ggplot2)
# unzip and read data
activity <- read.table(unz("activity.zip", "activity.csv"), header=T, sep=",", stringsAsFactors=FALSE)
# replace stringdates by dates
activity$date <- as.Date(activity[,2])
# define a function index for converting interval (HHMM) to intervalIndex (1:288)
index <- function (interv) {
     # split into HH and MM
     HH <- interv %/% 100
     MM <- interv %% 100
     MM/5 + HH*12 + 1
}
# and reverse function interv to time string
interv <- function (ind) {
     HH <- (ind-1) %/% 12
     MM <- (ind-1) %% 12 *5
     HH*100 + MM
     paste(sprintf("%02d", HH),":",sprintf("%02d", MM), sep = "")
}

# add a variable for interval index within each day
activity$intervalindex <- index(activity$interval)
# check in case missing values in dates and intervals (should both be 0)
sum(is.na(activity$date))
sum(is.na(activity$interval))

# filter out NA step counts, group by date and sum steps for each date
dayssums <- summarise(group_by(filter(activity, !is.na(steps)), date), daysum = sum(steps))
hist(dayssums$daysum, main="Distribution of steps/day", xlab="Steps/day")
mean(dayssums$daysum)
median(dayssums$daysum)

# group by interval, get average for each interval, plot and find interval with biggest mean
intervalAverages <- summarise(group_by(filter(activity, !is.na(steps)), intervalindex), intervalMean = mean(steps))
with (intervalAverages, plot(intervalindex, intervalMean, type="l", main="Average steps vs. intervalindex"))
maxint <- as.integer(arrange(intervalAverages, desc(intervalMean))[1,1])
maxint
# converting to HHMM format
interv(maxint)

# count missing values
sum(is.na(activity$steps))

# Strategy for filling in NAs: use the average for that interval
# Make a new dataset and loop through activity and fill in NAs
NAcount <- 0
newActivity <- activity

for (i in 1:nrow(newActivity)) {
     if (is.na (newActivity[i, 1])) {
          newActivity[i, 1] <- intervalAverages[newActivity[i,4], 2]
          NAcount <- NAcount + 1
          } 
     }
# check that all NAs are gone
sum(is.na(newActivity))

# make a new histogram of number of steps per day
dayssums2 <- summarise(group_by(newActivity, date), daysum = sum(steps))
par(mfrow=c(1,2))
hist(dayssums2$daysum, main="Distribution of steps/day imputed", xlab="Steps/day" )
hist(dayssums$daysum, main="Distribution of steps/day na.rm", xlab="Steps/day")
mean(dayssums2$daysum)
median(dayssums2$daysum)
mean(dayssums$daysum)
median(dayssums$daysum)

# put in a new factor variable weekday/weekend calculated from date (wday= 0 or 6 is weekend)
newActivity$day <- factor(ifelse(((as.POSIXlt(newActivity$date)$wday %% 6) == 0),"weekend", "weekday"))

# calculate intervalmeans
intervalAverages2 <- summarise(group_by(newActivity, day, intervalindex), intervalMean = mean(steps))

# plot interval means for weekdays and weekends
ggplot(intervalAverages2, aes(intervalindex, intervalMean)) +
     geom_line() +
     ggtitle("Mean steps per interval for weekdays and weekends") +
     facet_grid(day~.)
