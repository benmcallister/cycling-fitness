library(tidyverse)
library(plyr)
library(lubridate)
library("trackeR")

temp <- list.files(pattern = "*.tcx")

AllData <-
  list.files(pattern="*.tcx") %>% 
  map_df(~readTCX(.))

AllData$date <- as.Date(AllData$time)

last_wko_date <- max(AllData$date)

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
dailySummary$DiffHR <- dailySummary$heart_rate_round - dailySummary$heart_rate_median
dailySummary$Diffpower <- dailySummary$power_round - dailySummary$power_median
dailySummary$pHR <- dailySummary$power_median / dailySummary$heart_rate_median
dailySummary$wattMinutes <- dailySummary$power_median * dailySummary$duration

dailySummary

# create new variable - training day
dailySummary$TrainingDay <- dailySummary$date - min(dailySummary$date)
mutate(dailySummary, SessionNum = row_number())

# graph average power
AvgPowerGraph <- ggplot(dailySummary, aes(x=date, y=power_median)) +
  geom_col() + 
  xlab("")
AvgPowerGraph

# graph power minutes
wattMinutesGraph <- ggplot(dailySummary, aes(x=date, y=wattMinutes)) +
  geom_col() + 
  xlab("")
wattMinutesGraph

# graph average HR
AvgHrGraph <- ggplot(dailySummary, aes(x=date, y=heart_rate_median)) +
  geom_line() + 
  xlab("")
AvgHrGraph

# graph average HR
AvgRatioGraph <- ggplot(dailySummary, aes(x=date, y=pHR)) +
  geom_col() + 
  xlab("")
AvgRatioGraph


# linear model exploration
scatter.smooth(x=AllData$heart_rate, y=AllData$power, main="Power ~ Heart Rate")  

first_workout = AllData[AllData$date=="2022-02-22", ] # Data for first workout
dim(first_workout)
tail(first_workout)
scatter.smooth(x=first_workout$heart_rate, y=first_workout$power, main="Power ~ Heart Rate")  

last_wko = AllData[AllData$date==last_wko_date, ] # Data for last workout
dim(last_wko)
tail(last_wko)
scatter.smooth(x=last_wko$heart_rate, y=last_wko$power, main="Power ~ Heart Rate")  


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
