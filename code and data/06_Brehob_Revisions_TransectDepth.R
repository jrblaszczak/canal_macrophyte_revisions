## Brehob revisions - Transect depth variability

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#Read in Biomass Data
B <- read.csv("Environmental_and_Macrophyte_AllObservations.csv")