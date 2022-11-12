library("Stat2Data")
library("rStrava")
library("dplyr")
library("ggplot2")
library("lubridate")

# marathon data
data("Marathon")
marathon <- Marathon

# strava data
id <- "72136201"
app_name <- "Capstone Project"
app_client_id <- "96808"
app_secret <- "a30e98d43087e3e040d75f9ca06c8d3eecb6ec2f"
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))

myinfo <- get_athlete(stoken, id)
myinfo

myacts <- get_activity_list(stoken)
myacts

strava <- compile_activities(myacts, units="imperial")

# garmin data
garmin <- read.csv("Activities.csv")

# view dfs
View(marathon)
View(strava)
View(garmin)

# analyze marathon
marathon_idx <- which(marathon$Miles == "26.2")
marathon_idx
marathon_idx[1] # idx is 225
first_marathon <- marathon[marathon_idx[1],]
first_marathon
second_marathon <- marathon[marathon_idx[2],]
second_marathon

m1t <- slice(marathon, 0:marathon_idx[1]) # all training until first marathon
View(m1t)
m1t$Date <- mdy(m1t$Date)
m1t$Month <- as.factor(month(m1t$Date))
m1t$Short <- as.logical(m1t$Short)

m2t <- slice(marathon, 0:marathon_idx[2])
m2t <- slice(m2t, marathon_idx[1]+1:marathon_idx[2])
View(m2t)
m2t$Date <- mdy(m2t$Date)
m2t$Month <- as.factor(month(m2t$Date))
m2t$Short <- as.logical(m2t$Short)

## number of runs by month
par(mfrow=c(1,2))
ggplot(data=m1t, aes(x=factor(Month))) + geom_bar(stat="count", width=0.5, fill="steelblue") + 
  xlab("Month") + ylab("Number of Runs") + ggtitle("Number of Runs by Month") + theme_minimal()
ggplot(data=m2t, aes(x=factor(Month))) + geom_bar(stat="count", width=0.5, fill="steelblue") + 
  xlab("Month") + ylab("Number of Runs") + ggtitle("Number of Runs by Month") + theme_minimal()

## distance by date
ggplot(data=m1t, aes(x=Date, y=Miles, fill=Short)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Distance (mi)") + ggtitle("Distance by Date") + theme_minimal()
ggplot(data=m2t, aes(x=Date, y=Miles, fill=Short)) + geom_bar(stat="identity") +
  xlab("Month") + ylab("Distance (mi)") + ggtitle("Distance by Date") + theme_minimal()

## average speed by month
ggplot(data=m1t, aes(x=Month, y=PaceMin, fill=Month)) + geom_boxplot()+ xlab("Month") + ylab("Average Pace (mi/min)") +
  ggtitle("Average Pace by Month") + theme_minimal()
ggplot(data=m2t, aes(x=Month, y=PaceMin, fill=Month)) + geom_boxplot()+ xlab("Month") + ylab("Average Pace (mi/min)") +
  ggtitle("Average Pace by Month") + theme_minimal()
       