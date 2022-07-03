library(tidyverse)
library(lubridate)
library("trackeR")

# setwd("/Users/benmcallister/Documents/xrm-data-sci/cycling-fitness")

temp <- list.files(pattern = "*.tcx")
pathToData <- "./raw-data"

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
HRDist <- ggplot(selectData, aes(x = heart_rate, fill = heart_rate)) + 
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

summary(selectData)
sd(selectData$power)
sd(selectData$heart_rate, na.rm = TRUE)


# Initial exploratory plot
ggplot(selectData, aes(heart_rate, power, color = date)) + 
  geom_point(alpha = 0.05, shape = 3) +
  xlim(75, 175) +
  ylim(0, 175) + geom_smooth()

# Summary stats
medPower_All <- median(selectData$power)
medPower_All
medHR_All <- median(selectData$heart_rate, na.rm = TRUE)
medHR_All
min(selectData$date)
max(selectData$date)

pHR_All <-median(selectData$power) / median(selectData$heart_rate, na.rm = TRUE)

## BIG PLOT - ALL HR and POWER
allDataPlotBig <- ggplot(selectData, aes(heart_rate, power, color = minutes)) + 
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

 allDataPlotBig <- ggplot(selectData, aes(heart_rate, power, color = minutes)) + 
  geom_jitter(alpha = 0.08, shape = 3, width = 0.5) +
  scale_x_continuous("", expand = c(0,0), limits = c(80, 160)) +
  scale_y_continuous("", expand = c(0,0), limits = c(0, 160)) +
  scale_color_viridis_c()

allDataPlotBig + 
  geom_hline(aes(yintercept = median(power, na.rm = TRUE)),  linetype = "dotted", alpha = 0.8) + 
  geom_vline(aes(xintercept = median(heart_rate, na.rm = TRUE)),
             linetype = "dotted", alpha = 0.8) +
  theme(legend.position = "none", axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )

# Time series plot
ggplot(selectData, aes(elapsed_time, power, color = date)) + 
  geom_point(alpha = 0.1, shape = 3) +
  scale_x_continuous() + scale_y_continuous(limits = c(0, 160))  +
  scale_color_viridis_c("Date", labels = as.Date)

threeSessions <- selectData %>% filter(date %in% c("2022-06-01", "2022-06-08", "2022-06-11"))

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
medianDuration <- median(dailySummary$duration)

dailySummary
median(dailySummary$pHR, na.rm = TRUE )
median(dailySummary$median_power, na.rm = TRUE )
median(selectData$power, na.rm = TRUE)

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

# graph average HR
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


# graph Watt-Minute per Heartbeat
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



# select recent date and previous date to compare
beforeAfter <- selectData %>% filter(date == "2022-04-14" | 
                                       date == "2022-06-25") 

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

my_model <- lm(power ~ heart_rate, data = beforeAfter)
my_multi_mdoel <- lm(power ~ heart_rate*elapsed_time, data = beforeAfter)

sd(selectData$power)
sd(beforeAfter$power)


ggplot(beforeAfter, aes(x=heart_rate, color=SessionNum)) +
  geom_histogram(fill="white", position="dodge") +
  theme(legend.position="top")

ggplot(beforeAfter, aes(elapsed_time, heart_rate, color = SessionNum)) + 
  geom_line(alpha = 0.7) +
  scale_x_continuous() #+ scale_y_continuous(limits = c(0.8, 1.3))  

ggplot(beforeAfter, aes(elapsed_time, power, color = SessionNum)) + 
  geom_line(alpha = 0.7) +
  scale_x_continuous() + scale_y_continuous(limits = c(100, 150))  


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
