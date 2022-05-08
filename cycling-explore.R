library(tidyverse)
library(lubridate)
library("trackeR")

temp <- list.files(pattern = "*.tcx")

AllData <-
  list.files(pattern="*.tcx") %>% 
  map_df(~readTCX(.))

AllData$date <- as.Date(AllData$time)

selectData <- AllData %>% select(time, date, heart_rate, cadence_cycling, power)

last_wko_date <- max(selectData$date)

## would be interesting to do color as the elapsed duration of the workout
selectData <- selectData %>% group_by(date) %>% 
  mutate(elapsed_time = difftime(time, min(time), units = "mins")) %>%
  head(-150) %>% tail(-150) %>%       #trim first and last 150 observations per wko
  ungroup() %>% mutate(across(6, round, 2)) %>% 
  mutate(minutes = as.numeric((minutes = trunc(elapsed_time))))


# distribution of HR
HRDist <- ggplot(selectData, aes(x = heart_rate, fill = date)) + 
  geom_histogram(binwidth = 1) + 
  xlim(80, 160)  + 
  scale_y_continuous("") +
  geom_vline(aes(xintercept = median(heart_rate, na.rm = TRUE)), 
             linetype = "dotted", color = "grey") + 
  annotate("text", x = 136, y = 2000, label = "Median HR = 134", 
           vjust = 1, size = 3, color = "white", angle = 270, fontface = "bold") + 
  xlab("Heart Rate") + ylab("") +
  labs(title = "Frequency Distribution of Heart Rate", 
       subtitle = "All Rides, Feb 17 - May 1, 2022") +
  theme(plot.title = element_text(face="bold"),
        axis.text.y = element_blank())
HRDist 

powerDist <- ggplot(selectData, aes(x = power)) + 
  geom_histogram(binwidth = 1) + 
  xlim(60, 150) + 
  geom_vline(aes(xintercept = median(power, na.rm = TRUE)), 
             linetype = "dotted", color = "grey") + 
  annotate("text", x = 121, y = 1500, label = "Median Power = 118", 
           vjust = 1, size = 3, color = "white", angle = 270, fontface = "bold") + 
  xlab("Heart Rate") + ylab("") +
  labs(title = "Frequency Distribution of Power (Watts)", 
       subtitle = "All Rides, Feb 17 - May 1, 2022") +
  theme(plot.title = element_text(face="bold"),
        axis.text.y = element_blank())
powerDist


# Initial exploratory plot
ggplot(selectData, aes(heart_rate, power, color = date)) + 
  geom_point(alpha = 0.05, shape = 3) +
  xlim(75, 175) +
  ylim(0, 175) + geom_smooth()

# Summary stats
median(selectData$power)
median(selectData$heart_rate, na.rm = TRUE)
min(selectData$date)
max(selectData$date)

## BIG PLOT - ALL HR and POWER
allDataPlot <- ggplot(selectData, aes(heart_rate, power, color = minutes)) + 
  geom_jitter(alpha = 0.08, shape = 3, width = 0.5) +
  scale_x_continuous(name = "Heart Rate", expand = c(0,0), limits = c(80, 160)) +
  scale_y_continuous(name = "Power (Watts)", expand = c(0,0), limits = c(0, 160)) +
  scale_color_viridis_c()

allDataPlot + 
  geom_hline(aes(yintercept = median(power, na.rm = TRUE)),  linetype = "dotted", alpha = 0.8) + 
  geom_vline(aes(xintercept = median(heart_rate, na.rm = TRUE)),
             linetype = "dotted", alpha = 0.8) + 
  annotate("text", x = 91, y = 124, label = "Median Power = 118", 
           vjust = 1, size = 4, color = "grey40") + 
  annotate("text", x = 136, y = 47, label = "Median HR = 134", 
           vjust = 1, size = 4, color = "grey40", angle = 270) + 
  xlab("Heart Rate") + ylab("Power (Watts)") +
  labs(title = "Power vs Heart Rate", 
       subtitle = "All Rides, Feb 17 - May 1, 2022", 
       color = "Elapsed \nTime") +
  theme(plot.title = element_text(face="bold"))

# Time series plot
ggplot(selectData, aes(elapsed_time, power, color = date)) + 
  geom_point(alpha = 0.1, shape = 3) +
  scale_x_continuous() + scale_y_continuous(limits = c(0, 160))  +
  scale_color_viridis_c("Date", labels = as.Date)

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
dailySummary$pHR <- dailySummary$median_power / dailySummary$median_HR
dailySummary$wattMinutes <- dailySummary$median_power * dailySummary$duration
dailySummary$wattHours <- dailySummary$median_power * dailySummary$duration/60

dailySummary

# create new variable - training day
dailySummary$TrainingDay <- dailySummary$date - min(dailySummary$date) + 1
dailySummary <- mutate(dailySummary, SessionNum = row_number())

ggplot(dailySummary, aes(x=date, y=duration)) +
  geom_col() + xlab("") + 
  annotate("text", x = as.Date("2022-04-01"), y = 25, label = "(Spring Break)", 
           vjust = 1, size = 3, color = "grey40", fontface = "bold") + 
  labs(title = "Workout Duration", 
       subtitle = "All Rides, Feb 17 - May 1, 2022") +
  theme(plot.title = element_text(face="bold"))

# graph average power
AvgPowerGraph <- ggplot(dailySummary, aes(x=date, y=median_power)) +
  geom_col() + 
  xlab("")
AvgPowerGraph + 
  labs(title = "Median Power per Workout", 
       subtitle = "All Rides, Feb 17 - May 1, 2022",
       y = "Watts") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-04-01"), y = 25, label = "(Spring Break)", 
           vjust = 1, size = 3, color = "grey40", fontface = "bold")

# graph power minutes
wattMinutesGraph <- ggplot(dailySummary, aes(x=date, y=wattMinutes)) +
  geom_col() + 
  xlab("")
wattMinutesGraph

# graph average HR
AvgHrGraph <- ggplot(dailySummary, aes(x=date, y=median_HR)) +
  geom_col() + 
  xlab("")
AvgHrGraph + 
  labs(title = "Median Heart Rate per Workout", 
       subtitle = "All Rides, Feb 17 - May 1, 2022",
       y = "") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-04-01"), y = 25, label = "(Spring Break)", 
           vjust = 1, size = 3, color = "grey40", fontface = "bold")

# graph average HR
AvgRatioGraph <- ggplot(dailySummary, aes(x=date, y=pHR)) +
  geom_col() + 
  xlab("")
AvgRatioGraph + 
  labs(title = "Watt-Minute per Heartbeat across Workouts", 
       subtitle = "All Rides, Feb 17 - May 1, 2022",
       y = "") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-04-01"), y = 0.2, label = "(Spring Break)", 
           vjust = 1, size = 3, color = "grey40", fontface = "bold")


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
