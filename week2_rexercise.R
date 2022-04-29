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
head(wildschwein_BE_timelag)
summary(wildschwein_BE_timelag)
wildschwein_BE_countAnimals <- wildschwein_BE_timelag %>%
  group_by(TierName) %>%
  summarise(n=n())
#3 animals were tracked (Rosa, Ruth, Sabi)
wildschwein_BE_duration <- wildschwein_BE_timelag %>%
  dplyr::group_by(TierName) %>%
  summarise(min=min(DatetimeUTC),max=max(DatetimeUTC)) %>%
  mutate(duration = difftime(max, min,units="days"))


#Rosa has been tracked for 234.6663 days, Ruth for 261.6559 days, Sabi for 338.5834 days.
wildschwein_BE_gaps <- wildschwein_BE_timelag %>%
  filter(timelag > 10000) %>%
  group_by(TierID) %>%
  summarise(n=n())

ggplot(wildschwein_BE_timelag,aes(y=TierName,x=DatetimeUTC)) + geom_line()
ggplot(wildschwein_BE_timelag,aes(x=DatetimeUTC,y=timelag, col=TierID)) + geom_line()

#There have been around 400-500 gaps of more than 10'000 seconds for each boar (ca. 2.7h)
#They were tracked partially concurrently. First boar was Sabi, starting on 2014-08-22 21:00:12,
#also finishing last (longest tracking). Then Ruth and Rosa got tracked from the 2014-11-07 18:00:43,
# lasting until 2015-07-27 09:45:15 and 2015-06-29 23:45:11, respectively.