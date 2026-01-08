## Brehob revisions - Sediment analyses

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#Read in data
sed<-read.csv("Sed_Data_Summary_from_GitHub.csv", header = T)
View(sed)
colnames(sed) <- c("Event","Canal","Treatment","Date","Transect","Bulk_Density_g_mL","pct_OM","sed_pH")

#Replace canal names to match manuscript
sed$Canal <- revalue(sed$Canal, replace = c("A" = "A",
                                          "DRL" = "UL",
                                          "MSL" = "DL"))

#Make Canal and Event factors
sed$Event<-as.factor(sed$Event)
sed$Canal<-as.factor(sed$Canal)

#Test for Kruskal-Wallis differences
kruskal.test(pct_OM ~ Canal, data = sed) # p < 0.05
pairwise.wilcox.test(sed$pct_OM, sed$Canal, p.adjust.method = "BH", exact=F)

kruskal.test(pct_OM ~ Event, data = sed) # p = 0.96
