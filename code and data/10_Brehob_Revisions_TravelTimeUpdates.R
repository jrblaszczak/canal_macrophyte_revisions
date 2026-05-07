## Travel Time revisions

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

## Import original travel time files with mistakes
setwd("./original data/")
tt_files <- list.files(pattern = "*MetabModelingFlow.csv")

tt <- ldply(tt_files, function(filename) {
  dum <- read.csv(filename, header = TRUE)
  dum$ID <- filename
  return(dum)
})

setwd("..")

## Format canal
tt$Canal <- revalue(tt$ID, replace = c("AMetabModelingFlow.csv" = "A",
                                       "DRLMetabModelingFlow.csv" = "UL",
                                       "MSLMetabModelingFlow.csv" = "DL"))

## First, extract DO sensor reach length
## Second, remove old travel time calculation

# (1) Back calculate the length
tt$Length <- tt$Velocity*tt$TravelTime
# (2) TT was in days - multiply by the 86,400 seconds/day to get m
tt$Length <- tt$Length*86400
# (3) Create a travel time in hours
tt$TravelTime_seconds <- tt$Length/tt$Velocity
tt$TravelTime_hours <- tt$TravelTime_seconds/3600

## Calculate the mean travel time per canal
tt %>%
  group_by(Canal) %>%
  summarise_at(.vars = 'TravelTime_hours', .funs = mean)


