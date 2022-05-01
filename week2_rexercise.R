## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

## Task 2

wildschwein_BE_timelag <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="secs")))

##How many animals were tracked?
str(wildschwein_BE_timelag)
head(wildschwein_BE_timelag)
summary(wildschwein_BE_timelag)
wildschwein_BE_countObs <- wildschwein_BE_timelag %>%
  group_by(TierName) %>%
  summarise(n=n())
#3 animals were tracked (Rosa, Ruth, Sabi)
wildschwein_BE_duration <- wildschwein_BE_timelag %>%
  dplyr::group_by(TierName) %>%
  summarise(min=min(DatetimeUTC),max=max(DatetimeUTC)) %>%
  mutate(duration = difftime(max, min,units="days"))

#Rosa has been tracked for 234.6663 days, Ruth for 261.6559 days, Sabi for 338.5834 days.
ggplot(wildschwein_BE_timelag,aes(y=TierName,x=DatxetimeUTC)) + geom_line()

(wildschwein_BE_gaps <- wildschwein_BE_timelag %>%
  filter(timelag > 10000) %>%
  group_by(TierID) %>%
  summarise(n=n()))


ggplot(wildschwein_BE_timelag,aes(x=DatetimeUTC,y=timelag, col=TierID)) + geom_line()

#There have been around 400-500 gaps of more than 10'000 seconds for each boar (ca. 2.7h)
#They were tracked partially concurrently. First boar was Sabi, starting on 2014-08-22 21:00:12,
#also finishing last (longest tracking). Then Ruth and Rosa got tracked from the 2014-11-07 18:00:43,
# lasting until 2015-07-27 09:45:15 and 2015-06-29 23:45:11, respectively.

##Task 3
wildschwein_BE_distance <- wildschwein_BE_timelag %>%
  mutate(steplength = sqrt(abs((E-lead(E,1)^2) + (N-lead(N,1)^2)))) %>%
  mutate(speed = steplength/timelag)
#the unit is unit/duration in seconds

##Task 4
caro <- read_delim("caro60.csv")
caro_3 <- caro %>% dplyr::filter(row_number() %% 3 == 0)
caro_6 <- caro %>% dplyr::filter(row_number() %% 6 == 0)
caro_9 <- caro %>% dplyr::filter(row_number() %% 9 == 0)

caro_calc <- caro %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="secs"))) %>%
  mutate(steplength = sqrt(abs((E-lead(E,1)^2) + (N-lead(N,1)^2)))) %>%
  mutate(speed = steplength/timelag)

caro_3_calc <- caro_3 %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="secs"))) %>%
  mutate(steplength = sqrt(abs((E-lead(E,1)^2) + (N-lead(N,1)^2)))) %>%
  mutate(speed = steplength/timelag)

caro_6_calc <- caro_6 %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="secs"))) %>%
  mutate(steplength = sqrt(abs((E-lead(E,1)^2) + (N-lead(N,1)^2)))) %>%
  mutate(speed = steplength/timelag)

caro_9_calc <- caro_9 %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units="secs"))) %>%
  mutate(steplength = sqrt(abs((E-lead(E,1)^2) + (N-lead(N,1)^2)))) %>%
  mutate(speed = steplength/timelag)

ggplot(caro_calc,aes(DatetimeUTC,speed)) + geom_line(color="black") + geom_line(data=caro_3_calc, color="red") +
  theme_classic() + geom_line(data=caro_6_calc, color="orange") + geom_line(data=caro_9_calc,color="green")
#Legende krieg ich nicht hin und auch sonst scheint mir die Geschwindigkeit eine seltsame Zahl.

#Interpretation speed differences: with decreasing resolution, speed decreased as timelag between
#observations increased. Steplength experienced less deviations.

#Trajectories:
ggplot(caro_calc,aes(E,N)) + geom_path(color="black") + geom_path(data=caro_3_calc, color="red") +
  theme_classic()
ggplot(caro_calc,aes(E,N)) + geom_path(color="black") + geom_path(data=caro_6_calc, color="orange") +
  theme_classic()
ggplot(caro_calc,aes(E,N)) + geom_path(color="black") + geom_path(data=caro_9_calc, color="green") +
  theme_classic()
#Interpretation: sampling reduction decreased accuracy quite a lot. When only using every 6th or 9th observation,
# the real coordinates were lost on the way to a high degree. Especially observations that show circling around
#the same spots would not be visible anymore. The information about total range of motion is decreasing as well.
#The general direction is still recognizable though.