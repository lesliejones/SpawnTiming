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

fit <- lmer(Slope ~ MxDAT + sw_begin + (1|site), data=facetSL)
summary (fit)
