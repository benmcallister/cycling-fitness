library(tidyverse)
library(plyr)
library(lubridate)
library("trackeR")

temp <- list.files(pattern = "*.tcx")

AllData <-
  list.files(pattern="*.tcx") %>% 
  map_df(~readTCX(.))

AllData$date <- as.Date(AllData$time)

# function to calculate workout duration in minutes
find_duration <- function(times){
  interval(min(times), max(times)) / dminutes(1)
}

# summarize duration of each day's workout
dailyDuration <- AllData %>% group_by(date) %>% 
  summarize_at(vars(time), funs(duration = find_duration))

dailyPowerHR <- AllData %>% group_by(date) %>% 
  summarise_at(vars(power, heart_rate), 
               list(~ round(mean(., na.rm = TRUE)), ~ median(., na.rm = TRUE)))

dailySummary <- merge(dailyDuration, dailyPowerHR)

# don't know why this one doesn't work
#dailyMeans <- AllData %>% group_by(date) %>% summarie(mean(power), mean(heart_rate, na.rm = TRUE))
# dailyMeans <- rename(dailyMeans, meanPower = "mean(power)", meanHR = "mean(heart_rate, na.rm = TRUE)")
dailyMeans$pHR <- dailyMeans$power / dailyMeans$heart_rate

# create new variable - training day
dailyMeans$TrainingDay <- dailyMeans$date - min(dailyMeans$date)
mutate(dailyMeans, SessionNum = row_number())

# graph average power
AvgPowerGraph <- ggplot(dailyMeans, aes(x=date, y=power)) +
  geom_col() + 
  xlab("")
AvgPowerGraph

# graph average HR
AvgHrGraph <- ggplot(dailyMeans, aes(x=date, y=heart_rate)) +
  geom_line() + 
  xlab("")
AvgHrGraph

# graph average HR
AvgRatioGraph <- ggplot(dailyMeans, aes(x=date, y=pHR)) +
  geom_line() + 
  xlab("")
AvgRatioGraph


# linear model exploration
scatter.smooth(x=AllData$heart_rate, y=AllData$power, main="Power ~ Heart Rate")  

first_workout = AllData[AllData$date=="2022-02-22", ] # Data for first workout
dim(first_workout)
tail(first_workout)
scatter.smooth(x=first_workout$heart_rate, y=first_workout$power, main="Power ~ Heart Rate")  

first_wko_lm = lm(heart_rate ~ power, data=first_workout)
summary(first_wko_lm)

# turn into a function
my.power.lm = function(date.df) {
  coef(lm(heart_rate ~ power, data=date.df))
}
my.power.lm(first_workout) # New way
coef(first_wko_lm)

power.coefs.d = ddply(AllData, .(date), my.power.lm)
head(power.coefs.d) # Get back a data frame
power.coefs.d
