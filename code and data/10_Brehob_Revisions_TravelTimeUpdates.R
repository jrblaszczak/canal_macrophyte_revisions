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

## Remove old travel time calculation








