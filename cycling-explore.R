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
powerMeans <- AllData %>% group_by(date) %>% summarize(mean(power))
powerMeans <- rename(powerMeans, meanPower = "mean(power)")

# graph average power
AvgPowerGraph <- ggplot(powerMeans, aes(x=date, y=meanPower)) +
  geom_col() + 
  xlab("")
AvgPowerGraph

allGraph <- ggplot(AllData, aes(x=time, y=power)) +
  geom_line() + 
  xlab("")
allGraph




# filepath <- system.file("extdata/tcx/", "2022-03-26_14-00-19", package = "trackeR")
cycleFeb <- readTCX(file = "2022-02-17_05-54-47.tcx", timezone = "GMT")
cycleMarch24 <- readTCX(file = "1437503.tcx", timezone = "GMT")
cycleMarch26 <- readTCX(file = "2022-03-26_14-00-19.tcx", timezone = "GMT")


head(cycleMarch24)

mean(cycleMarch$power)
mean(cycleFeb$power)
str(cycleMarch)

cycleMarch$powerHR <- cycleMarch$power / cycleMarch$heart_rate

marchGraph <- ggplot(cycleMarch, aes(x=time, y=heart_rate)) +
  geom_line() + 
  xlab("")
marchGraph

cycleFeb$powerHR <- cycleFeb$power / cycleFeb$heart_rate

febGraph <- ggplot(cycleFeb, aes(x=time, y=heart_rate)) +
  geom_line() + 
  xlab("")
febGraph
