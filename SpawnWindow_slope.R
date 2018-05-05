library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

spawn <- read.csv("Data/spawnwindow.csv")
View(spawn)

slope <- spawn %>% filter(!is.na(Temperature)) %>% group_by(Site, year) %>%
  do({
    mod = lm(Temperature ~ DayOfYear, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })

View(slope)
write.csv(slope, "C:/PSMFC_Chinook/SpawnTiming/Data/slope_spawnwindows.csv")
