#Models and Plots of Whole Canal Metabolism Results
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
B <- B[,c("Canal","Event","Biomass_per_Area")] # biomass has more measurements and therefore needs to be averaged separately

B$Canal <- as.factor(B$Canal); B$Event <- as.factor(B$Event)

B_summary <- B %>%
  group_by(Canal, Event) %>%
  summarise(across(Biomass_per_Area, .fns = c(mean, sd), na.rm = TRUE))
colnames(B_summary) <- c("Canal","Event","Mean_Biomass", "SD_Biomass")




##### Plot time series #####
#Plot of GPP
g1<-ggplot(data=df, aes(x=Date, y=GPP50, color=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=GPP2.5, ymax=GPP97.5), size=0.5,width = 0.2)+
  scale_color_manual("Canal", values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = "horizontal",
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size=10))+
  coord_cartesian(ylim = c(0, 40), clip = "off")
  #annotate("segment", x = subset(biomass, Canal=="A")$Date, xend = subset(biomass, Canal=="A")$Date, y = 0, yend = 12, colour = NA, size=1, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  #annotate("segment", x = subset(biomass, Canal=="UL")$Date, xend = subset(biomass, Canal=="UL")$Date, y = 0, yend = 12, colour = NA, size=1, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  #annotate("segment", x = subset(biomass, Canal=="DL")$Date, xend = subset(biomass, Canal=="DL")$Date, y = 0, yend = 12, colour = NA, size=1, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))
g1+theme(legend.position = "bottom")

#Plot of ER
g2<-ggplot(data=df, aes(x=Date, y=ER50, color=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=ER2.5, ymax=ER97.5), size=0.5, width = 0.5)+
  scale_color_manual("Canal", values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("ER (g ", O[2], " ", m^-2, " ", d^-1,")")))+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  #annotate("segment", x = subset(biomass, Canal=="A")$Date, xend = subset(biomass, Canal=="A")$Date, y = 0, yend = 10, colour = "#6393A6", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  #annotate("segment", x = subset(biomass, Canal=="UL")$Date, xend = subset(biomass, Canal=="UL")$Date, y = 0, yend = 10, colour = "#733B36", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  #annotate("segment", x = subset(biomass, Canal=="DL")$Date, xend = subset(biomass, Canal=="DL")$Date, y = 0, yend = 10, colour = "#BF785E", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  coord_cartesian(ylim = c(-40,0), clip = "off")
g2

##Combine graphs
g1noleg <- g1 + theme(legend.position = "none")#Make graph 1 without it's legend
leg <- get_legend(g1)#Create an object of the legend from graph 1
graph1<-plot_grid(g1noleg, NULL, g2, rel_heights = c(1, -0.05, 1.15), ncol=1, align="v", scale=0.95)#Plot just the two graphs (no legend) so that I can align the y axes
plot_grid(graph1, NULL, leg, nrow = 3, rel_heights = c(1, -0.05, 0.15))#Add legend in to final plot



##### NEP and P/R #####
df$PRratio<-abs(df$GPP50/df$ER50)#Calculate production to respiration ratio

##Plot P/R time series
ggplot(data=df, aes(x=Date, y=PRratio, col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("P/R Ratio")))+
  theme(panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        legend.text = element_text(size=10))

##Create new data frame for plotting NEP and biomass per area
newdf<-data.frame(df$Date, df$Canal, df$NEP)
newdf$mean_Biomass_per_Area<-NA
newdf$sd_Biomass_per_Area<-NA
colnames(newdf)<-c("Date","Canal","NEP","meanBPA","sdBPA")
# add in wcb data
wcb$Date<-as.POSIXct(wcb$Date, format="%m/%d/%Y")
newbiomass<-data.frame(wcb$Date, wcb$Canal, wcb$ave_Biomass_per_Area, wcb$sd_Biomass_per_Area)
newbiomass$NEP<-NA
colnames(newbiomass)<-c("Date","Canal","meanBPA","sdBPA","NEP")
plotdf<-rbind(newdf,newbiomass)

##Plot NEP and biomass
ggplot(data=plotdf, aes(x=Date, y=NEP, col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  #geom_line()+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("NEP (g ", O[2], " ", m^-2, " ", d^-1,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title = element_text(size=12))+
  geom_bar(aes(x=Date, y=meanBPA/2, fill=Canal), stat = 'identity')+
  #labs(y=expression(paste("Biomass per area (g/", m^2,")")))+
  scale_fill_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+ #add colors that I want
  geom_errorbar(aes(ymin=meanBPA/2-sdBPA/2, ymax=meanBPA/2+sdBPA/2), width=.2,position=position_dodge(.9),stat="identity", color="black")+
  scale_y_continuous(limits = c(-15,18),
                     sec.axis = sec_axis(~.*2, name=expression(paste("Biomass per area (g / ", m^2,")"))))


##########################
## Figure 3 for paper
##########################

plot_grid(
plot_grid(

graph1,

plot_grid(
  
  ggplot(data=plotdf, aes(x=Date, y=NEP, col=Canal))+
    geom_point(size=3.5)+theme_bw()+
    scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
    labs(y=expression(paste("NEP (g ", O[2], " ", m^-2, " ", d^-1,")")))+
    geom_hline(yintercept = 0, size = 0.75)+
    theme(panel.grid.minor = element_blank(),
          axis.text=element_text(size=12),
          legend.text = element_text(size=12),
          legend.title = element_text(size=14),
          axis.title = element_text(size=12),
          axis.title.x = element_blank(),
          legend.position = "none"),
  
ggplot(data = plotdf, aes(x = Date, y = meanBPA))+
  geom_point(aes(color=Canal), stat = 'identity', size = 4.5)+
  #geom_line(aes(group=Canal))+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  geom_errorbar(aes(ymin=meanBPA-sdBPA, ymax=meanBPA+sdBPA, color = Canal),
                width = 0.2,
                position=position_dodge(0.9),
                stat="identity", size = 1)+
  labs(y = expression(paste("Biomass per area (g / ", m^2,")")))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title = element_text(size=12),
        legend.position = "none"),

align = "hv", nrow = 2, labels = c("B","C")),

nrow = 1, ncol = 2, align = "h", labels = c("A","")),

NULL, leg, nrow = 3, rel_heights = c(1, -0.05, 0.15))















