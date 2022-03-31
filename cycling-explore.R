library(tidyverse)
library("trackeR")

temp <- list.files(pattern = "*.tcx")

AllData <-
  list.files(pattern="*.tcx") %>% 
  map_df(~readTCX(.))

AllData$date <- as.Date(AllData$time)

# Function to find single-set PRs with time window and exercise as variable
mean_power <- function(df){
  df %>% group_by(date) %>% filter(power == mean(power)) %>% 
    select(date, power) %>% arrange(date)
}

mean_power(AllData)

# find average ppower for each date
dailyMeans <- AllData %>% group_by(date) %>% summarize(mean(power), mean(heart_rate, na.rm = TRUE))
dailyMeans <- rename(dailyMeans, meanPower = "mean(power)", meanHR = "mean(heart_rate, na.rm = TRUE)")
dailyMeans$pHR <- dailyMeans$meanPower / dailyMeans$meanHR

# graph average power
AvgPowerGraph <- ggplot(dailyMeans, aes(x=date, y=meanPower)) +
  geom_col() + 
  xlab("")
AvgPowerGraph

# graph average HR
AvgHrGraph <- ggplot(dailyMeans, aes(x=date, y=meanHR)) +
  geom_line() + 
  xlab("")
AvgHrGraph

# graph average HR
AvgRatioGraph <- ggplot(dailyMeans, aes(x=date, y=pHR)) +
  geom_line() + 
  xlab("")
AvgRatioGraph

