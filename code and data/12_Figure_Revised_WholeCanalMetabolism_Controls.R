#This code will model and plot the results of the whole-stream metabolism modeling for each of the canals.
#Updates 8/17/26 by JRB

## Import packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

## Import data
wclt<-read.csv("WholeCanalMetabolism_LightTemp_DailyAverages.csv")

#####################
## Plot Figure 4
#####################

GPPlight_plot <- ggplot(data=wclt, aes(x=PAR, y=GPP50, col=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=GPP2.5, ymax=GPP97.5), size=0.5)+
  scale_color_manual(values = c("A" = "#E69F00","UL" = "#0072B2", "DL" = "#009E73"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")), 
       x=expression(paste("Mean daily surface PAR (μmol ", m^-2," ", s^-1,")")))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size=12, color = "black"),
        axis.title = element_text(size=14, color = "black"),
        legend.text = element_text(size=12, color = "black"),
        legend.title = element_text(size=14, color = "black"), legend.position = "bottom")

GPPlight_plot

ERtemp_plot <- ggplot(data=wclt, aes(x=Temp, y=abs(ER50), col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=abs(ER2.5), ymax=abs(ER97.5)), size=0.5)+
  scale_color_manual(values = c("A" = "#E69F00","UL" = "#0072B2", "DL" = "#009E73"))+
  labs(y=expression(paste("|ER| (g ", O[2], " ", m^-2, " ", d^-1,")")), x="Mean daily water temperature (°C)")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size=12, color = "black"),
        axis.title = element_text(size=14, color = "black"),
        legend.text = element_text(size=12, color = "black"),
        legend.title = element_text(size=14, color = "black"))
ERtemp_plot

legend <- cowplot::get_plot_component(GPPlight_plot, 'guide-box-bottom', return_all = TRUE)

plot_grid(
  plot_grid(GPPlight_plot+theme(legend.position = "none"),
            ERtemp_plot+theme(legend.position = "none"),
            nrow = 1, ncol = 2, align = "hv", labels = c("A","B")),
  cowplot::ggdraw(legend),
  nrow = 2, rel_heights = c(1, 0.15))



