#Plot_Macrophyte_Density_and_Composition
#Meredith Brehob
#5/9/2022
#This code will plot macrophyte density and composition for each sampling event.
#Updates by JRB

## Import packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse","ggtext"), require, character.only=T)

##### Read in Data #####
canal_aves <- read.csv("Macrophyte_Density_SiteAverages.csv")
canal_sp_aves <- read.csv("Macrophyte_Composition_SiteAverages.csv")

canal_all_data <- read.csv("Environmental_and_Macrophyte_AllObservations.csv")

## Rename events
canal_all_data$Event_name <- revalue(as.factor(canal_all_data$Event),
                                 c("1" = "Early summer",
                                   "2" = "Mid summer",
                                   "3" = "Late summer"))
canal_sp_aves$Event_name <- revalue(as.factor(canal_sp_aves$Event),
                                     c("1" = "Early summer",
                                       "2" = "Mid summer",
                                       "3" = "Late summer"))



##### Macrophyte Density Plot #####
## Colors chosen from Okabe-Ito Palette (https://www.audioeye.com/post/colorblind-friendly-palettes/)
fill.order <- factor(canal_all_data$Canal, levels = c('UL', 'DL', 'A'))#creating fill order so that it is in correct sampling order

g1 <- ggplot(canal_all_data, aes(x = Canal, y = Biomass_per_Area, fill = fill.order))+
  geom_boxplot()+
  geom_jitter(size = 1.5, width = 0.1)+
  facet_grid(~ Event_name)+
  theme_bw(base_size = 12)+
  theme(strip.background=element_rect(fill="white"))+
  scale_fill_manual("Canal", values = c("A" = "#E69F00","UL" = "#0072B2", "DL" = "#009E73"))+
  labs(y=expression(paste("Biomass per area (g AFDM / ", m^2,")")))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size=12),
        axis.text = element_text(size=12, color = "black"),
        axis.title.y = element_text(size=14, color = "black"),
        axis.title.x = element_blank())
g1



##### Proportion of Macrophyte Composition Plot #####
# change ditchgrass to Grass-like SAV
canal_sp_aves$Plant_Type[which(canal_sp_aves$Plant_Type == "Ditchgrass")] <- "Grass-like SAV"
canal_sp_aves$Plant_Type[which(canal_sp_aves$Plant_Type == "Curly Leaf Pondweed")] <- "Curly leaf pondweed"
canal_sp_aves$Plant_Type[which(canal_sp_aves$Plant_Type == "Filamentous Algae")] <- "Filamentous algae"

g2<-
  ggplot(canal_sp_aves, aes(x = Canal, y = Biomass_per_Area, fill = Plant_Type)) + 
  geom_bar(stat = 'identity', position = 'fill') + facet_grid(~ Event_name)+
  labs(y=expression(paste("Plant type or algae proportion (%)")), fill="Plant type") +
  theme_bw(base_size = 12)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.background=element_rect(fill="white"),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size=12, color = "black"),
        axis.title = element_text(size=14, color = "black"),
        legend.text = element_markdown())+
  scale_y_continuous(labels = function(x) paste0(x * 100, '%'), expand = c(0.005,0.005))+
  scale_fill_manual("Autotrophic\nbiomass type",values=c("#F2E0C9", "#FEC10D", "#E6860A","#2B3715", "#75954F","#786F65"),
                    labels = c("Curly leaf pondweed", "*Elodea*","Filamentous algae",
                               "Grass-like SAV","Milfoil","*Nitella*"))
g2





##### Combine Plots #####
plot_grid(g1, g2, ncol = 1, align = "hv", labels = "AUTO")


