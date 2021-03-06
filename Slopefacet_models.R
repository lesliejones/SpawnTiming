library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lme4)


facetSL <- read.csv("Data/Mergeslopefacets.csv")
View(facetSL)

#MxDAT = maximum daily average temp
#MA7d_DAT = maximum 7 day average temp
#sw_begin = julian day 1st day of spawn window

Mx <- lme(Slope ~ MxDAT + sw_begin, random = ~1|site, data=facetSL)
summary(Mx)

MA7d_DAT <- lme(Slope ~ MA7d_DAT + sw_begin, random = ~1|site, data=facetSL)
summary(MA7d_DAT)

#since run timing is an average response, maybe it does make sense to look at 
# average maximum temperature values for the streams?

facetSL %>%
  group_by(site) %>%
  summarize(meanMx = mean(MxDAT),
            meanSlope = mean(Slope)) %>%
  lm(meanSlope ~ meanMx, data = .) %>% 
  summary

lme(Slope ~ MxDAT, random = ~1|site, data = facetSL) %>% summary



# MWMT and date of MWMT ---------------------------------------------------

dat <- read_csv("data/spawn_timseries.csv")
dat %>%
  group_by(Site, year) %>%
  summarize(min = min(Date), max = max(Date))

dat <- dat %>% filter(!is.na(Temperature))

library(caTools)

#7-day moving average of daily average temps
dat$WAT <- runmean(dat$Temperature, k = 7, endrule = "NA", align = "center") 

mwat <- dat %>%
  group_by(Site, year) %>%
  summarize(WAT = max(WAT, na.rm = TRUE))

mwat <- left_join(mwat, dat)
mwat.mn <- mwat %>%
  group_by(Site) %>%
  summarize(mn.WAT = mean(WAT),
            md.WAT = median(WAT))

#variability in WAT with mean (red triangle) and median (blue star)
ggplot(mwat, aes(x = WAT, y = Site)) +
  geom_point() +
  geom_point(data = mwat.mn, aes(x = mn.WAT, y = Site),
             color = "red", shape = 6) +
  geom_point(data = mwat.mn, aes(x = md.WAT, y = Site),
             color = "blue", shape = 8)

ggplot(mwat, aes(x = year, y = WAT)) +
  geom_point() +
  facet_wrap(~Site)


ggplot(mwat, aes(x = WAT, y = year)) +
  geom_point() 

#merge spwindow days datafram
mwat <- merge(x=mwat, y=spdays, by=c("Site"), all.x=TRUE)
#calculate jd_dif
mwat$jd_dif = (mwat$sw_mid - mwat$DayOfYear)

mwatmodel <- lm(jd_dif ~ WAT, data=mwat)
summary(mwatmodel)
