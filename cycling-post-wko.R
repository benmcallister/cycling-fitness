library(tidyverse)
library(lubridate)
library("trackeR")

temp <- list.files(pattern = "*.tcx")

AllData <-
  list.files(pattern="*.tcx") %>% 
  map_df(~readTCX(.))

# Add a date field
AllData$date <- as.Date(AllData$time)

# Throw out variables I don't collect on spin bike
selectData <- AllData %>% select(time, date, heart_rate, cadence_cycling, power)
selectData$efficiency <- selectData$power / selectData$heart_rate

## Add variable for elapsed time in minutes rounded to 2 decimal points
## Also add a column with just a round number of elapsed minutes
selectData <- selectData %>% group_by(date) %>% 
  mutate(elapsed_time = difftime(time, min(time), units = "mins"), #elapsed minutes
         elapsed_seconds = difftime(time, min(time), units = "secs"), #elapsed seconds
         row_id = row_number()) %>%    #add row ID
  #head(-150) %>% tail(-150) %>%       #trim first and last 150 observations per wko
  ungroup() %>% mutate(across(6, round, 2)) %>% 
  mutate(minutes = as.numeric((minutes = trunc(elapsed_time))))

# Create a "median" workout
medianRide <- selectData %>% group_by(row_id) %>% 
  summarize(median_power = median(power, na.rm = TRUE),
            median_HR = median(heart_rate, na.rm = TRUE),
            n = n())

medianRide$efficiency <- medianRide$median_power / medianRide$median_HR

# Get date of most recent workout
last_wko_date <- max(selectData$date)
last_wko_date

lastRide <- selectData %>% filter(date == last_wko_date)
lastRideRows <- max(lastRide$row_id)
lastRideLimits <- c(0, lastRideRows)

# Historical Summary stats
medPowerAll <- median(selectData$power, na.rm = TRUE)
medPowerAll
powerAnnotationAll <- paste("Median Power \n(All Rides) = ", medPowerAll)
medPowerLastRide <- median(lastRide$power, na.rm = TRUE)
medPowerLastRide
powerAnnotationLast <- paste("Median Power \n(Last Ride) = ", medPowerLastRide)
powerSubtitle <- paste(last_wko_date, " vs. Median Ride")

medHRAll <- median(selectData$heart_rate, na.rm = TRUE)
medHRLast <- median(lastRide$heart_rate, na.rm = TRUE)
HRAnnotationAll <- paste("Median HR \n(All Rides) = ", medHRAll)
HRAnnotationLast <- paste("Median HR \n(Last Ride) = ", medHRLast)

powerPlot <- ggplot(lastRide, aes(row_id, power, color = "Last Ride")) + 
  geom_point(alpha = 0.15, shape = 3, position = "jitter") + 
  geom_smooth(data = medianRide, aes(y = median_power, color = "Median Ride"), 
            alpha = 0.5) + #labs(color = "hello") +
  xlim(lastRideLimits) + ylim(80, 150)

powerPlot +
  geom_hline(aes(yintercept = medPowerAll),  
             linetype = "dashed", alpha = 0.9, color = "#00BFC4") + 
  annotate("text", x = 3000, y = 115, label = powerAnnotationAll, 
          size = 3, color = "grey50", fontface = "bold") + 
  geom_hline(aes(yintercept = medPowerLastRide),  
             linetype = "solid", alpha = 1, color = "#F8766D") + 
  annotate("text", x = 400, y = 129, label = powerAnnotationLast, 
           size = 3, color = "grey50", fontface = "bold") + 
  xlab("Time") + ylab("Power (Watts)") +
  labs(title = "Time Series: Power", 
       subtitle = powerSubtitle, 
       color = "") +
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )

HRPlot <- ggplot(lastRide, aes(row_id, heart_rate, color = "Last Ride")) + 
  geom_point(alpha = 0.1, shape = 4) + 
  geom_smooth(data = medianRide, aes(y = median_HR, color = "Median Ride"), 
              alpha = 0.5) + 
  xlim(lastRideLimits) + ylim(90, 150)

HRPlot +
  geom_hline(aes(yintercept = medHRAll),  
             linetype = "dashed", alpha = 0.9, color = "#00BFC4") + 
  annotate("text", x = 400, y = 135, label = HRAnnotationAll, 
           size = 3, color = "grey50", fontface = "bold") + 
  geom_hline(aes(yintercept = medHRLast),  
             linetype = "solid", alpha = 1, color = "#F8766D") + 
  annotate("text", x = 300, y = 126, label = HRAnnotationLast, 
           size = 3, color = "grey50", fontface = "bold") + 
  xlab("Time") + ylab("Heart Rate") +
  labs(title = "Time Series: Heart Rate", 
       subtitle = powerSubtitle, 
       color = "") +
  theme(plot.title = element_text(face="bold"), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )


ggplot(lastRide, aes(row_id, efficiency)) + 
  geom_point(position = "jitter", alpha = 0.3, shape = 2) + 
  geom_smooth(data = medianRide, aes(y = efficiency, color = "red"), 
              alpha = 0.5) + labs(color = "Median Ride") +
  xlim(lastRideLimits) + ylim(0.8, 1.1)


# Calculate Summary stats
medPower_All <- median(selectData$power)
medPower_All
medHR_All <- median(selectData$heart_rate, na.rm = TRUE)
medHR_All

### CREATE THE DAILY SUMMARY DATAFRAME

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

# merge Duration and Power + HR data frames, add some other vars
dailySummary <- merge(dailyDuration, dailyPowerHR)
dailySummary$pHR <- dailySummary$median_power / dailySummary$median_HR
dailySummary$wattMinutes <- dailySummary$median_power * dailySummary$duration
dailySummary$wattHours <- dailySummary$median_power * dailySummary$duration/60
medianDuration <- median(dailySummary$duration)

dailySummary
median(dailySummary$pHR, na.rm = TRUE )
median(dailySummary$median_power, na.rm = TRUE )
median(selectData$power, na.rm = TRUE)

# create new variable - training day
dailySummary$TrainingDay <- dailySummary$date - min(dailySummary$date) + 1
dailySummary <- mutate(dailySummary, SessionNum = row_number())

# DURATION PLOT
ggplot(dailySummary, aes(x=date, y=duration)) +
  geom_col() + xlab("") + 
  annotate("text", x = as.Date("2022-04-01"), y = 25, label = "(Spring Break)", 
           vjust = 1, size = 3, color = "grey40", fontface = "bold") + 
  labs(title = "Workout Duration", 
       subtitle = "All Rides, Feb 17 - May 1, 2022") +
  theme(plot.title = element_text(face="bold"))

# MEDIAN POWER PLOT
AvgPowerGraph <- ggplot(dailySummary, aes(x=date, y=median_power)) +
  geom_col() + 
  xlab("")
AvgPowerGraph + 
  labs(title = "Median Power per Workout", 
       subtitle = "All Rides, Feb 17 - June 25, 2022",
       y = "Watts") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-04-01"), y = 70, label = "(Spring Break)", 
           vjust = 1, size = 2, color = "grey40")  + 
  annotate("text", x = as.Date("2022-05-15"), y = 70, label = "COVID-19", 
           vjust = 1, size = 4, color = "grey40", fontface = "bold") + 
  geom_hline(aes(yintercept = medPower_All),  linetype = "dotted", alpha = 0.8) + 
  annotate("text", x = as.Date("2022-05-15"), y = 125, label = "median = 119", 
           vjust = 1, size = 3, color = "grey40")

# graph power minutes
wattMinutesGraph <- ggplot(dailySummary, aes(x=date, y=wattMinutes)) +
  geom_col() + 
  xlab("")
wattMinutesGraph

# MEDIAN HR PLOT
AvgHrGraph <- ggplot(dailySummary, aes(x=date, y=median_HR)) +
  geom_col() + 
  xlab("")
AvgHrGraph + 
  labs(title = "Median Heart Rate per Workout", 
       subtitle = "All Rides, Feb 17 - June 25, 2022",
       y = "") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-04-01"), y = 70, label = "(Spring Break)", 
           vjust = 1, size = 2, color = "grey40") + 
  annotate("text", x = as.Date("2022-05-16"), y = 70, label = "COVID-19", 
           vjust = 1, size = 4, color = "grey40", fontface = "bold") + 
  geom_hline(aes(yintercept = medHR_All),  linetype = "dotted", alpha = 0.8) + 
  annotate("text", x = as.Date("2022-05-16"), y = 140, label = "median HR = 133", 
           vjust = 1, size = 3, color = "grey40")


# WATT MINUNTE PER HEARTBEAT
AvgRatioGraph <- ggplot(dailySummary, aes(x=date, y=pHR)) +
  geom_col() + 
  xlab("")
AvgRatioGraph + 
  labs(title = "Watt-Minute per Heartbeat per Workout", 
       subtitle = "All Rides, Feb 17 - June 25, 2022",
       y = "") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-04-01"), y = 0.59, label = "Spring \nBreak", 
           vjust = 1, size = 3, color = "grey40", fontface = "bold") + 
  annotate("text", x = as.Date("2022-05-16"), y = 0.57, label = "COVID-19", 
           vjust = 1, size = 4, color = "grey40", fontface = "bold") + 
  geom_hline(aes(yintercept = pHR_All),  linetype = "dotted", alpha = 0.8) + 
  annotate("text", x = as.Date("2022-05-16"), y = 0.94, label = "median = 0.9", 
           vjust = 1, size = 3, color = "grey40")

# PLOT DURATION DISTRIBUTION
durationDist <- ggplot(dailySummary, aes(x = duration, fill = duration)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(aes(xintercept = 30), 
             linetype = "dashed", color = "red") + 
  annotate("text", x = 29, y = 5, label = "30 minutes", 
           vjust = 1, size = 3, color = "red", angle = 90, fontface = "bold") + 
  geom_vline(aes(xintercept = 40), 
             linetype = "dashed", color = "red") + 
  annotate("text", x = 39, y = 5, label = "40 minutes", 
           vjust = 1, size = 3, color = "red", angle = 90, fontface = "bold") + 
  geom_vline(aes(xintercept = 45), 
             linetype = "dashed", color = "red") + 
  annotate("text", x = 44, y = 5, label = "45 minutes", 
           vjust = 1, size = 3, color = "red", angle = 90, fontface = "bold") + 
  geom_vline(aes(xintercept = 50), 
             linetype = "dashed", color = "red") + 
  annotate("text", x = 49, y = 5, label = "50 minutes", 
           vjust = 1, size = 3, color = "red", angle = 90, fontface = "bold") + 
  geom_vline(aes(xintercept = 60), 
             linetype = "dashed", color = "red") + 
  annotate("text", x = 59, y = 5, label = "60 minutes", 
           vjust = 1, size = 3, color = "red", angle = 90, fontface = "bold") + 
  xlab("Workout Duration (one-minute increments)") + ylab("Number of Workouts") +
  labs(title = "Frequency Workout Duration (n = 34)", 
       subtitle = "All Rides, Feb 17 - July 9, 2022") +
  theme(plot.title = element_text(face="bold"))
durationDist 

# BEFORE AFTER COMPARISON
beforeAfter <- selectData %>% filter(date == "2022-04-14" | 
                                       date == "2022-08-03") 

beforeAfter <- mutate(beforeAfter, SessionNum = as.factor(date))
beforeAfter$pHR <- beforeAfter$power / beforeAfter$heart_rate

bA_meds <- beforeAfter %>% group_by(date) %>% 
  summarize(med_HR = median(heart_rate, na.rm = TRUE), med_Power = median(power))


# mu <- beforeAfter %>% group_by(SessionNum) %>% summarize(medHR = median(heart_rate))


ggplot(beforeAfter, aes(x = heart_rate, fill = SessionNum)) + 
  geom_histogram(alpha = 0.5, position = "dodge") + 
  geom_vline(aes(xintercept = median(selectData$heart_rate, na.rm = TRUE)),  linetype = "solid", alpha = 0.5) + 
  geom_vline(aes(xintercept = 139), linetype = "dotted", alpha = 0.8) + 
  geom_vline(aes(xintercept = 130), linetype = "dotted", alpha = 0.8) + 
  xlim(85, 160)

ggplot(beforeAfter, aes(x = power, fill = SessionNum)) + 
  geom_histogram(alpha = 0.5, position = "dodge") + xlim(85, 150) + 
  geom_vline(aes(xintercept = medPower_All),  linetype = "solid", alpha = 0.5) + 
  geom_vline(aes(xintercept = 132), linetype = "dotted", alpha = 0.8) + 
  geom_vline(aes(xintercept = 127), linetype = "dotted", alpha = 0.8) + 
  xlim(85, 160)  + 
  annotate("text", x = 91, y = 124, label = "Median Power = 118", 
           vjust = 1, size = 4, color = "grey40") + 
  annotate("text", x = 136, y = 47, label = "Median HR = 134", 
           vjust = 1, size = 4, color = "grey40", angle = 270)

beforeAfterPlotBig <- ggplot(beforeAfter, aes(heart_rate, power, color = SessionNum)) + 
  geom_jitter(alpha = 0.3, shape = 3, width = 0.5) +
  scale_x_continuous(name = "Heart Rate", expand = c(0,0), limits = c(110, 160)) +
  scale_y_continuous(name = "Power (Watts)", expand = c(0,0), limits = c(110, 160)) 

beforeAfterPlotBig + 
  geom_hline(aes(yintercept = medPower_All),  linetype = "solid", alpha = 0.8) + 
  geom_vline(aes(xintercept = medHR_All), linetype = "solid", alpha = 0.8) + 
  annotate("text", x = 150, y = 117, label = "Median Power (all rides) = 119", 
           vjust = 0, size = 3, color = "grey40", fontface = "bold") + 
  annotate("text", x = 131.5, y = 148, label = "Median HR (all rides) = 134", 
           vjust = 1, size = 3, color = "grey40", angle = 90, fontface = "bold") + 
  xlab("Heart Rate") + ylab("Power (Watts)") +
  labs(title = "Power vs Heart Rate", 
       subtitle = "Pre-COVID (April 14) and Post-COVID (June 25)", 
       color = "Date") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(data= bA_meds, aes(yintercept = med_Power,col=as.factor(date)), linetype = "longdash") +
  geom_vline(data= bA_meds, aes(xintercept = med_HR,col=as.factor(date)), linetype = "longdash")


# time series exploration
latestSession <- selectData %>% filter(date == max(selectData$date))

threeSessions <- selectData %>% filter(date == "2022-02-22" | 
                                         date == "2022-03-22" | 
                                         date == "2022-06-25")

ggplot(threeSessions, aes(elapsed_time, power, color = date)) + 
  geom_point(alpha = 0.1, shape = 2) +
  scale_x_continuous() + scale_y_continuous(limits = c(0, 160))  


ggplot(threeSessions, aes(elapsed_time, heart_rate, color = power)) + 
  geom_point(alpha = 0.6, shape = 3) +
  scale_x_continuous() + scale_y_continuous(limits = c(60, 160)) +
  scale_color_gradient(low="blue", high="red", limits = c(80, 160))
