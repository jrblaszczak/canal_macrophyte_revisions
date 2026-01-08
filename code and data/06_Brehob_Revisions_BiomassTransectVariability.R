## Brehob revisions - Biomass transect variability

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#Read in Biomass Data
B <- read.csv("Environmental_and_Macrophyte_AllObservations.csv")
B <- B[,c("Canal","Event","Biomass_per_Area")] # biomass has more measurements and therefore needs to be averaged separately

B$Canal <- as.factor(B$Canal); B$Event <- as.factor(B$Event)

B_summary <- B %>%
  group_by(Canal, Event) %>%
  summarise(across(Biomass_per_Area, .fns = c(mean, sd), na.rm = TRUE))
colnames(B_summary) <- c("Canal","Event","Mean_Biomass", "SD_Biomass")
B_summary$CV <- B_summary$SD_Biomass/B_summary$Mean_Biomass

write.csv(B_summary, "Biomass_Transect_Variability.csv")

B %>%
  group_by(Canal, Event) %>%
  summarise_all(~ sum(!is.na(.)))


