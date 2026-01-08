#Plot_Macrophyte_Density_and_Composition
#Meredith Brehob
#5/9/2022
#This code will plot macrophyte density and composition for each sampling event.
#Updates 4/29/24 by JRB

## Import packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

##### Read in Data #####
canal_aves <- read.csv("../Data_Final/Macrophyte_Density_SiteAverages.csv")
canal_sp_aves <- read.csv("../Data_Final/Macrophyte_Composition_SiteAverages.csv")

canal_all_data <- read.csv("../Data_Final/Environmental_and_Macrophyte_AllObservations.csv")

## Rename events
canal_all_data$Event_name <- revalue(as.factor(canal_all_data$Event),
                                 c("1" = "Early Summer",
                                   "2" = "Mid Summer",
                                   "3" = "Late Summer"))
canal_sp_aves$Event_name <- revalue(as.factor(canal_sp_aves$Event),
                                     c("1" = "Early Summer",
                                       "2" = "Mid Summer",
                                       "3" = "Late Summer"))



##### Macrophyte Density Plot #####
fill.order <- factor(canal_all_data$Canal, levels = c('UL', 'DL', 'A'))#creating fill order so that it is in correct sampling order

g1 <- ggplot(canal_all_data, aes(x = Canal, y = Biomass_per_Area, fill = fill.order))+
  geom_boxplot()+
  geom_jitter(size = 1.5, width = 0.1)+
  facet_grid(~ Event_name)+
  theme_bw(base_size = 12)+
  theme(strip.background=element_rect(fill="white"))+
  scale_fill_manual("Canal", values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("Biomass per area (g AFDM ", m^-2,")")))+
  theme(axis.title.x = element_blank())
g1


##### Proportion of Macrophyte Composition Plot #####
# change ditchgrass to Grass-like SAV
canal_sp_aves$Plant_Type[which(canal_sp_aves$Plant_Type == "Ditchgrass")] <- "Grass-like SAV"

g2<-
  ggplot(canal_sp_aves, aes(x = Canal, y = Biomass_per_Area, fill = Plant_Type)) + 
  geom_bar(stat = 'identity', position = 'fill') + facet_grid(~ Event_name)+
  labs(y=expression(paste("Plant type or algae proportion (%)")),fill="Plant Type") +
  theme_bw(base_size = 12)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.background=element_rect(fill="white"))+
  scale_y_continuous(labels = function(x) paste0(x * 100, '%'), expand = c(0.005,0.005))+
  scale_fill_manual("Autotrophic\nBiomass Type",values=c("#F2E0C9", "#FEC10D", "#E6860A","#2B3715", "#75954F","#786F65"))
g2

##### Combine Plots #####
plot_grid(g1, g2, ncol = 1, align = "hv", labels = "AUTO")


