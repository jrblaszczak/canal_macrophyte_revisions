# Figure 3 - Whole Canal Metabolism & Biomass Variation
# MB and updated by JRB

## Import packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

######################
## Import Data
######################
# Daily Metab
df <- read.csv("WholeCanalMetabolism_DailyValues.csv", header = T)
#Create NEP column
df$NEP50 <- df$GPP50 + df$ER50
#Make Canal a factor
df$Canal<-as.factor(df$Canal)
df$Date<-as.POSIXct(df$Date, format="%m/%d/%Y")

# Biomass
B <- read.csv("Environmental_and_Macrophyte_AllObservations.csv")
B <- B[,c("Canal","Event","Date","Biomass_per_Area")] # biomass has more measurements and therefore needs to be averaged separately
B$Canal <- as.factor(B$Canal); B$Event <- as.factor(B$Event)
B$Date <- as.POSIXct(B$Date, format = "%m/%d/%Y")
B_summary <- B %>%
  group_by(Canal, Event, Date) %>%
  summarise(across(Biomass_per_Area, .fns = c(mean, sd), na.rm = TRUE))
colnames(B_summary) <- c("Canal","Event","Date","Mean_Biomass", "SD_Biomass")


#################################
## Figure 3 - GPP and ER time series
#################################
#Plot of GPP
g1<-ggplot(data=df, aes(x=Date, y=GPP50, color=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=GPP2.5, ymax=GPP97.5), size=0.5,width = 0.2)+
  scale_color_manual("Canal", values = c("A" = "#E69F00","UL" = "#0072B2", "DL" = "#009E73"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        axis.title.x=element_blank(),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14))+
  coord_cartesian(ylim = c(0, 40), clip = "off")+
  scale_x_date(limits = as.Date(c("2021-04-01", "2021-08-10")),
               date_breaks = "1 month", date_labels = "%b")
g1+theme(legend.position = "bottom")


#Plot of ER
g2<-ggplot(data=df, aes(x=Date, y=ER50, color=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=ER2.5, ymax=ER97.5), size=0.5, width = 0.5)+
  scale_color_manual("Canal", values = c("A" = "#E69F00","UL" = "#0072B2", "DL" = "#009E73"))+
  labs(y=expression(paste("ER (g ", O[2], " ", m^-2, " ", d^-1,")")))+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14))+
  annotate("segment", x = subset(B_summary, Canal=="A")$Date, xend = subset(B_summary, Canal=="A")$Date, y = 0, yend = 10, colour = "#E69F00", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  annotate("segment", x = subset(B_summary, Canal=="UL")$Date, xend = subset(B_summary, Canal=="UL")$Date, y = 0, yend = 10, colour = "#0072B2", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  annotate("segment", x = subset(B_summary, Canal=="DL")$Date, xend = subset(B_summary, Canal=="DL")$Date, y = 0, yend = 10, colour = "#009E73", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  coord_cartesian(ylim = c(-40,0), clip = "off")+
  scale_x_date(limits = as.Date(c("2021-04-01", "2021-08-10")),
               date_breaks = "1 month", date_labels = "%b")
g2+theme(legend.position = "bottom")

##Combine graphs
g1noleg <- g1 + theme(legend.position = "none")#Make graph 1 without it's legend
leg <- get_legend(g1)#Create an object of the legend from graph 1
graph1<-plot_grid(g1noleg, NULL, g2, rel_heights = c(1, -0.05, 1.15), ncol=1, align="v", scale=0.95)#Plot just the two graphs (no legend) so that I can align the y axes
graph1

plot_grid(graph1, NULL, leg, nrow = 3, rel_heights = c(1, -0.05, 0.15))#Add legend in to final plot

##########################
## Figure 3 for paper
##########################

# NEP panel
NEP_panel <- ggplot(data=df, aes(x=Date, y=NEP50, col=Canal))+
  geom_point(size=3)+theme_bw()+
  scale_color_manual(values = c("A" = "#E69F00","UL" = "#0072B2", "DL" = "#009E73"))+
  labs(y=expression(paste("NEP (g ", O[2], " ", m^-2, " ", d^-1,")")))+
  geom_hline(yintercept = 0, size = 0.75)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        axis.title.x = element_blank(),
        legend.position = "none")+
  coord_cartesian(ylim = c(-17, 8), clip = "off")+
  scale_x_date(limits = as.Date(c("2021-04-01", "2021-08-10")),
               date_breaks = "1 month", date_labels = "%b")
NEP_panel

# Biomass panel
Biomass_panel <- ggplot(data = B_summary, aes(x = Date, y = Mean_Biomass))+
  geom_point(aes(color=Canal), stat = 'identity', size = 3)+
  scale_color_manual(values = c("A" = "#E69F00","UL" = "#0072B2", "DL" = "#009E73"))+
  geom_errorbar(aes(ymin=Mean_Biomass-SD_Biomass, ymax=Mean_Biomass+SD_Biomass, color = Canal),
                width = 0.2,
                position=position_dodge(0.9),
                stat="identity", size = 1)+
  labs(y = expression(paste("Biomass (g /"," ", m^2,")")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text=element_text(size=12),
        axis.title = element_text(size=14),
        legend.position = "none")+
  scale_x_date(limits = as.Date(c("2021-04-01", "2021-08-10")),
               date_breaks = "1 month", date_labels = "%b")


graph2<-plot_grid(NEP_panel, NULL, Biomass_panel,
                  rel_heights = c(1, -0.05, 1.15), ncol=1,
                  align="v", scale=0.95, labels = c("B","C"))
graph2

########################################
## Figure 3 all together
########################################

plot_grid(plot_grid(graph1, graph2, nrow = 1, ncol = 2, align = "hv", labels = c("A","")),
          NULL, leg, nrow = 3, rel_heights = c(1, -0.05, 0.15))




