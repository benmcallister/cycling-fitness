library(tidyverse)
# library(plyr)
library(lubridate)
library("trackeR")

temp <- list.files(pattern = "*.tcx")

AllData <-
  list.files(pattern="*.tcx") %>% 
  map_df(~readTCX(.))

AllData$date <- as.Date(AllData$time)

selectData <- AllData %>% select(time, date, heart_rate, cadence_cycling, power)

last_wko_date <- max(selectData$date)

# distribution of HR
HRDist <- ggplot(selectData, aes(x = heart_rate)) + 
  geom_histogram(binwidth = 1) + 
  xlim(60, 190)  + 
  geom_vline(aes(xintercept = median(heart_rate, na.rm = TRUE)))
HRDist

powerDist <- ggplot(selectData, aes(x = power)) + 
  geom_histogram(binwidth = 1) + 
  xlim(60, 175) + 
  geom_vline(aes(xintercept = median(power, na.rm = TRUE)))
powerDist

mean(selectData$power)

ggplot(selectData, aes(heart_rate, power, color = date)) + 
  geom_point(alpha = 0.05, shape = 3) +
  xlim(75, 175) +
  ylim(0, 175) + geom_smooth()

## would be interesting to do color as the elapsed duration of the workout
df <- selectData %>% group_by(date) %>% mutate(elapsed_time = time - min(time)) %>% ungroup()
selectData <- selectData %>% group_by(date) %>% 
  mutate(elapsed_time = difftime(time, min(time), units = "mins")) %>%
  ungroup() %>% mutate(across(6, round, 2))

selectData <- selectData %>% mutate(minutes = as.numeric((minutes = round(elapsed_time))))

ggplot(selectData, aes(heart_rate, power, color = minutes)) + 
  geom_point(alpha = 0.05, shape = 3) +
  xlim(75, 175) +
  ylim(0, 175) 

# function to calculate workout duration in minutes
find_duration <- function(times){
  interval(min(times), max(times)) / dminutes(1)
}

# summarize duration of each day's workout
dailyDuration <- selectData %>% group_by(date) %>% 
  summarize_at(vars(time), list(duration = find_duration))

dailyPowerHR <- selectData %>% group_by(date) %>% 
  summarize_at(vars(power, heart_rate), 
               list( ~ median(., na.rm = TRUE))) %>%
  rename(median_power = power, median_HR = heart_rate)

dailySummary <- merge(dailyDuration, dailyPowerHR)
dailySummary$DiffHR <- dailySummary$heart_rate_round - dailySummary$heart_rate_median
dailySummary$Diffpower <- dailySummary$power_round - dailySummary$power_median
dailySummary$pHR <- dailySummary$power_median / dailySummary$heart_rate_median
dailySummary$wattMinutes <- dailySummary$power_median * dailySummary$duration
dailySummary$wattHours <- dailySummary$power_median * dailySummary$duration/60

dailySummary

# create new variable - training day
dailySummary$TrainingDay <- dailySummary$date - min(dailySummary$date) + 1
dailySummary <- mutate(dailySummary, SessionNum = row_number())

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
  geom_col() + 
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
