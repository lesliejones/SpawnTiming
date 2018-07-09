# find maximum value in temperature timeseries by Site and year - retaining julian day

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lme4)
library(nlme)


temp <- read.csv("Data/spawn_timseries.csv")
temp <- filter(temp, Temperature != "NA")
View(temp)

require (dplyr)
temp_max <- temp %>% group_by(Site, year) %>% filter(Temperature = max(Temperature))
View(temp_max)

maxdata <- merge(x=temp_max, y=temp, by= c("Site", "year", "Temperature"), all.x = TRUE)
View(maxdata)


#rename vars for model merge
maxdata <- rename(maxdata, maxtemp = 'Temperature')
maxdata <- rename(maxdata, maxDOY = 'DayOfYear')
maxdata  %>% select(Site, maxtemp, maxDOY)

#import julian days for spawning windows
spdays <- read.csv("Data/spawnwindow_dates.csv")
spdays <- rename(spdays, Site='site')
View(spdays)

#merge spawn window days to maxtemp dataset

maxdata <- merge(x=maxdata, y=spdays, by=c("Site"), all.x=TRUE)

#calculate julian day difference between maximum temperature in timeseries and midpoint of spawning window
maxdata$jd_dif <- (maxdata$sw_mid - maxdata$maxDOY)
View(maxdata)

#plots
ggplot(maxdata, aes(x = jd_dif, y = maxtemp)) +
  geom_point() +
  facet_wrap(~Site)

#maxtemp model

maxmodel <- lme(maxtemp ~ jd_dif, random = ~1|year, data=maxdata)
summary(maxmodel)












