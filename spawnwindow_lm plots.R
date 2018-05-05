library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
setwd("C:/PSMFC_Chinook/Temp_RT")


library(tidyverse)

spawn <- read_csv("C:/PSMFC_Chinook/Temp_RT/spawnwindow_edit.csv")
View(spawn)

ggplot(spawn, aes(x=DayOfYear, y=Temperature)) + 
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) +
  geom_smooth (method = "lm", se = TRUE) + facet_wrap(~Site) +
  labs(title="Stream temperature",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

StempsRT <- spawn

#LOESS SMOOTHED CURVES
#Anchor only
Sitedf <- StempsRT[ which(StempsRT$Site=='Anchor'),]

plot23 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=153, xmax=237, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) +
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Anchor Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Beaver only
Sitedf <- StempsRT[ which(StempsRT$Site=='Beaver'),]

plot24 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=206, xmax=259, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Beaver Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Chuitna only
Sitedf <- StempsRT[ which(StempsRT$Site=='Chuitna'),]

plot25 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=206, xmax=251, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Chuitna Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Deception only
Sitedf <- StempsRT[ which(StempsRT$Site=='Deception'),]

plot26 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=197, xmax=237, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Deception Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Deep only
Sitedf <- StempsRT[ which(StempsRT$Site=='Deep'),]

plot27 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=183, xmax=251, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Deep Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Deshka only
Sitedf <- StempsRT[ which(StempsRT$Site=='Deshka'),]

plot28 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=197, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Deshka Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#EF Chulitna only
Sitedf <- StempsRT[ which(StempsRT$Site=='EF Chulitna'),]

plot29 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=189, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="EF Chulitna Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Fish only
Sitedf <- StempsRT[ which(StempsRT$Site=='Fish'),]

plot30 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=214, xmax=268, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Fish",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Little Willow only
Sitedf <- StempsRT[ which(StempsRT$Site=='Little Willow'),]

plot31 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=197, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Little Willow Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Montana only
Sitedf <- StempsRT[ which(StempsRT$Site=='Montana'),]

plot32 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=197, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Montana Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Moose (Su) only
Sitedf <- StempsRT[ which(StempsRT$Site=='Moose (Su)'),]

plot33 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=183, xmax=259, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Moose (Su) Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#NF Campbell only
Sitedf <- StempsRT[ which(StempsRT$Site=='NF Campbell'),]

plot34 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=183, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Montana Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Ninilchik only
Sitedf <- StempsRT[ which(StempsRT$Site=='Ninilchik'),]

plot35 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=197, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Ninilchik Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Rabbit only
Sitedf <- StempsRT[ which(StempsRT$Site=='Rabbit'),]

plot36 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=183, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Rabbit Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Resurrection only
Sitedf <- StempsRT[ which(StempsRT$Site=='Resurrection'),]

plot37 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=197, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Resurrection Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Russian only
Sitedf <- StempsRT[ which(StempsRT$Site=='Russian'),]

plot38 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=214, xmax=251, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Russian Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Ship only
Sitedf <- StempsRT[ which(StempsRT$Site=='Ship'),]

plot39 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=183, xmax=244, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Ship Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Stariski only
Sitedf <- StempsRT[ which(StempsRT$Site=='Stariski'),]

plot40 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=183, xmax=237, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Stariski Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Wasilla only
Sitedf <- StempsRT[ which(StempsRT$Site=='Wasilla'),]

plot41 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=214, xmax=274, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Wasilla Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))

#Willow only
Sitedf <- StempsRT[ which(StempsRT$Site=='Willow'),]

plot42 <- ggplot(Sitedf, aes(x=DayofYear, y=temperature)) + 
  geom_rect(data=NULL, aes(xmin=197, xmax=228, ymin=-Inf, ymax=Inf), fill="lightblue") +
  geom_smooth (aes(group=year, colour=year), method = "lm", se = FALSE) + 
  geom_smooth (method = "lm", se = TRUE) +
  labs(title="Stream temperature",
       subtitle="Willow Site",
       y="Stream temperature (C)",
       x="Date", theme_bw(base_size=15))


MyPlots = list(plot23, plot24,plot25, plot26, plot27,plot28,plot29, plot30, plot31, plot32,
               plot33, plot34,plot35, plot36,plot37,plot38, plot39, plot40, plot41, plot42)
pdf("spawnwindowplots.pdf")
MyPlots
dev.off()