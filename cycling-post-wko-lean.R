library(tidyverse)
library(lubridate)
library("trackeR")

# Read only the most recent TCX file
directory <- getwd()
files <- file.info(list.files(directory, pattern = "*.tcx"))
lastRideFile <- readTCX(rownames(files)[order(files$mtime)][nrow(files)])

# Read the previous Daily Summary
previousDailySummary <- read.csv("daily-summary.csv")

# Read all the data
# AllData <-
#  list.files(pattern="*.tcx") %>% 
#  map_df(~readTCX(.))

# Add a date field
lastRideFile$date <- as.Date(lastRideFile$time)

# Throw out variables I don't collect on spin bike
leanLastRide <- lastRideFile %>% select(time, date, heart_rate, cadence_cycling, power)
leanLastRide$efficiency <- leanLastRide$power / leanLastRide$heart_rate

## Add variable for elapsed time in minutes rounded to 2 decimal points
## Also add a column with just a round number of elapsed minutes
leanLastRide <- leanLastRide %>% group_by(date) %>% 
  mutate(elapsed_time = difftime(time, min(time), units = "mins"), #elapsed minutes
         elapsed_seconds = difftime(time, min(time), units = "secs"), #elapsed seconds
         row_id = row_number()) %>%    #add row ID
  #head(-150) %>% tail(-150) %>%       #trim first and last 150 observations per wko
  ungroup() %>% mutate(across(6, round, 2)) %>% 
  mutate(minutes = as.numeric((minutes = trunc(elapsed_time))))

# Get date of most recent workout
last_wko_date <- max(leanLastRide$date)
last_wko_date

# Crete dataframe for most recent ride
lastRide <- selectData %>% filter(date == last_wko_date)
lastRideRows <- max(lastRide$row_id)
lastRideLimits <- c(0, lastRideRows)

# simple graph of most recent ride - REQUIRE HR
ggplot(leanLastRide, aes(elapsed_time, power)) + 
  geom_point(aes(y = power)) + 
  geom_line(linetype = "dotted") + 
  geom_line(aes(y = heart_rate), linetype = "solid", color = "red")

ggplot(leanLastRide, aes(elapsed_time, heart_rate)) + 
  geom_point(color = "red", alpha=0.05)

## Good thru here


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
medianEnergy <- median(dailySummary$wattMinutes)


dailySummary
median(dailySummary$pHR, na.rm = TRUE )
median(dailySummary$median_power, na.rm = TRUE )
median(selectData$power, na.rm = TRUE)

summarySubtitle <- paste("All Rides,", 
                         format(min(selectData$date), format="%b %d"), 
                         "-", 
                         format(max(selectData$date), format="%b %d %Y"))

# create new variable - training day
dailySummary$TrainingDay <- dailySummary$date - min(dailySummary$date) + 1
dailySummary <- mutate(dailySummary, SessionNum = row_number())

lastRideSummary <- dailySummary %>% filter(date == last_wko_date)
medEnergyLast <- lastRideSummary$wattMinutes



# DURATION PLOT
ggplot(dailySummary, aes(x=date, y=duration)) +
  geom_point() + ylab("Minutes") + xlab("") + 
  annotate("text", x = as.Date("2022-03-31"), y = 25, label = "(Spring Break)",
           angle = 90,
           vjust = 1, size = 3, color = "grey40", fontface = "bold") + 
  annotate("text", x = as.Date("2022-05-15"), y = 15, label = "COVID-19", 
           vjust = 1, size = 3, color = "grey40", fontface = "bold") + 
  labs(title = "Workout Duration", 
       subtitle = summarySubtitle) +
  theme(plot.title = element_text(face="bold")) + 
  geom_point(data = dailySummary[which.max(dailySummary$date), ], 
             shape = 21, color="#F8766D", size = 2, stroke = 2)


# MEDIAN POWER PLOT
AvgPowerGraph <- ggplot(dailySummary, aes(x=date, y=median_power)) +
  geom_point() + 
  xlab("")
AvgPowerGraph + 
  labs(title = "Median Power per Workout", 
       subtitle = summarySubtitle,
       y = "Watts") +
  ylim(75, 185) +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-03-31"), y = 119, label = "(Spring Break)", 
           vjust = 1, size = 3, color = "grey40", angle = 90,)  + 
  annotate("text", x = as.Date("2022-05-14"), y = 125, label = "COVID-19", 
           vjust = 1, size = 3, color = "grey40") + 
  geom_hline(aes(yintercept = medPowerAll),  linetype = "dotted", alpha = 0.8) + 
  annotate("segment", x = as.Date("2022-03-01"), y = 145, 
           xend = as.Date("2022-03-01"), yend = medPowerAll,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")), 
           alpha = 0.4) + 
  annotate("text", x = as.Date("2022-03-01"), y = (149), label = paste("median =", medPowerAll), 
           vjust = 1, size = 3, color = "grey40") + 
  geom_point(data = lastRideSummary, 
             aes(date, median_power), 
             color="red", size = 2) +
  geom_label(data = lastRideSummary, 
             aes(date, median_power, label = median_power), 
             color="red", nudge_x = -7) +
  annotate("text", x = as.Date("2022-07-15"), y = (182), label = "Tabata Ride", 
           vjust = 1, size = 3, color = "grey40") 



# graph power minutes
wattMinutesGraph <- ggplot(dailySummary, aes(x=date, y=wattMinutes)) +
  geom_point() + 
  xlab("")
wattMinutesGraph + 
  labs(title = "Total Energy per Workout", 
       subtitle = summarySubtitle,
       y = "Watt-Minutes") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-05-14"), y = 4500, label = "COVID-19", 
           vjust = 1, size = 3, color = "grey40") + 
  geom_hline(aes(yintercept = medianEnergy),  linetype = "dotted", alpha = 0.8) + 
  annotate("segment", x = as.Date("2022-03-01"), y = 145, 
           xend = as.Date("2022-03-01"), yend = medPowerAll,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")), 
           alpha = 0.4) + 
  annotate("text", x = as.Date("2022-07-01"), y = (medianEnergy+350), 
           label = paste("median =", round(medianEnergy)), 
           vjust = 1, size = 3, color = "grey40") + 
  geom_point(data = lastRideSummary, 
             aes(date, wattMinutes), 
             color="red", size = 2) +
  geom_text(data = lastRideSummary, 
             aes(date, wattMinutes, label = round(wattMinutes)), 
             color="red", nudge_y = 400) 

dailySummary$wattMinutes 
rank(-dailySummary$wattMinutes)

which(rank(-dailySummary$wattMinutes)==1)
dailySummary[22,]

# MEDIAN HR PLOT
AvgHrGraph <- ggplot(dailySummary, aes(x=date, y=median_HR)) +
  geom_point() + 
  xlab("")
AvgHrGraph + 
  labs(title = "Median Heart Rate per Workout", 
       subtitle = summarySubtitle,
       y = "") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-03-31"), y = 131, 
           label = "(Spring Break)", vjust = 1, size = 3, 
           color = "grey40", angle = 90, fontface = "bold") + 
  annotate("text", x = as.Date("2022-05-16"), y = 128, 
           label = "COVID-19", vjust = 1, size = 3, 
           color = "grey40") + 
  geom_hline(aes(yintercept = medHRAll),  
             linetype = "dotted", alpha = 0.8) + 
  annotate("text", x = as.Date("2022-05-16"), y = 150, 
           label = HRAnnotationAll, vjust = 1, size = 3, 
           color = "grey50", fontface = "bold") + 
  geom_point(data = dailySummary[which.max(dailySummary$date), ], 
             color="#F8766D") +
  annotate("segment", x = as.Date("2022-05-15"), y = 145, 
         xend = as.Date("2022-05-15"), yend = medHRAll,
         arrow = arrow(type = "closed", length = unit(0.02, "npc")), 
         alpha = 0.4) +
  geom_label(data = lastRideSummary, 
             aes(date, median_HR, label = median_HR), 
             color="red", nudge_y = 4)


# WATT MINUNTE PER HEARTBEAT
AvgRatioGraph <- ggplot(subset(dailySummary, duration > 10), aes(x=date, y=pHR)) +
  geom_point() + 
  xlab("")
AvgRatioGraph + 
  labs(title = "Workout Efficiency: Watt-Minute per Heartbeat", 
       subtitle = summarySubtitle,
       y = "") +
  theme(plot.title = element_text(face="bold")) + 
  annotate("text", x = as.Date("2022-05-16"), 
           y = 0.91, 
           label = "COVID-19", 
           size = 4, 
           color = "grey60") + 
  geom_hline(aes(yintercept = medEfficiencyAll),  
             linetype = "dotted", 
             alpha = 0.8) + 
  annotate("text", x = as.Date("2022-08-05"), y = 0.84, 
           label = paste("median =", medEfficiencyAll), 
           size = 4, color = "grey40") + 
  annotate("segment", x = as.Date("2022-08-05"), y = 0.85, 
           xend = as.Date("2022-08-05"), yend = medEfficiencyAll,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")), 
           alpha = 0.4) +
  geom_point(data = dailySummary[which.max(dailySummary$date), ], 
             color="#F8766D",
             size = 4) +
  geom_label(data = lastRideSummary, 
             aes(date, pHR, label = round(pHR, 2)), 
             color="red", nudge_y = .01, size = 5) 
  
rank(-dailySummary$pHR)
tail(dailySummary)

# Write the summary data to a .csv
write.csv(dailySummary, file = "daily-summary.csv")
