library(tidyverse)
library("trackeR")
# filepath <- system.file("extdata/tcx/", "2022-03-26_14-00-19", package = "trackeR")
cycleDF <- readTCX(file = "2022-03-26_14-00-19.tcx", timezone = "GMT")

mean(cycleDF$power)
str(cycleDF)

p <- ggplot(cycleDF, aes(x=time, y=power)) +
  geom_line() + 
  xlab("")
p
