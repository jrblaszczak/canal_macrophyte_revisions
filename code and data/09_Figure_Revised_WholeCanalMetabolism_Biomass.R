#Models and Plots of Whole Canal Metabolism Results
# MB and updated by JRB

## Import packages
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

#Read in data
df<-read.csv("WholeCanalMetabolism_DailyValues.csv")
df$Date<-as.POSIXct(df$Date, format="%m/%d/%Y")
biomass<-read.csv("Macrophyte_Density_SiteAverages.csv")
biomass$Date<-as.POSIXct(biomass$Date, format="%m/%d/%Y")
wcb<-read.csv("../Data_Final/WholeCanal_Compartment_Metabolism_Biomass_SiteAverages.csv")
wclt<-read.csv("../Data_Final/WholeCanalMetabolism_LightTemp_DailyAverages.csv")


##### Site variation #####
df$Canal<-as.factor(df$Canal)
## GPP
kruskal.test(GPP50 ~ Canal, data = df)
pairwise.wilcox.test(df$GPP50, df$Canal, p.adjust.method = "BH", exact=F)
## ER
kruskal.test(ER50 ~ Canal, data = df)
pairwise.wilcox.test(df$ER50, df$Canal, p.adjust.method = "BH", exact=F)


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
  annotate("segment", x = subset(biomass, Canal=="A")$Date, xend = subset(biomass, Canal=="A")$Date, y = 0, yend = 10, colour = "#6393A6", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  annotate("segment", x = subset(biomass, Canal=="UL")$Date, xend = subset(biomass, Canal=="UL")$Date, y = 0, yend = 10, colour = "#733B36", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  annotate("segment", x = subset(biomass, Canal=="DL")$Date, xend = subset(biomass, Canal=="DL")$Date, y = 0, yend = 10, colour = "#BF785E", size=0.8, arrow = arrow(ends = "both", angle = 45, length = unit(.2,"cm")))+
  coord_cartesian(ylim = c(-40,0), clip = "off")
g2

##Combine graphs
g1noleg <- g1 + theme(legend.position = "none")#Make graph 1 without it's legend
leg <- get_legend(g1)#Create an object of the legend from graph 1
graph1<-plot_grid(g1noleg, NULL, g2, rel_heights = c(1, -0.05, 1.15), ncol=1, align="v", scale=0.95)#Plot just the two graphs (no legend) so that I can align the y axes
plot_grid(graph1, NULL, leg, nrow = 3, rel_heights = c(1, -0.05, 0.15))#Add legend in to final plot


##### Plot GPP vs ER #####
ggplot(data=df, aes(x=GPP50, y=abs(ER50), col=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=abs(ER2.5), ymax=abs(ER97.5)), size=0.75)+
  geom_errorbarh(aes(xmin=GPP2.5, xmax=GPP97.5))+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  xlim(c(0,40))+ylim(c(0,40))+
  labs(y=expression(paste("|ER| (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

##Correlation between GPP and ER
cor(x = df$GPP50, y = df$ER50, use = "complete.obs", method="kendall")
cor(x = subset(df, Canal=="A")$GPP50, y = subset(df, Canal=="A")$ER50, use = "complete.obs", method="kendall")
cor(x = subset(df, Canal=="UL")$GPP50, y = subset(df, Canal=="UL")$ER50, use = "complete.obs", method="kendall")
cor(x = subset(df, Canal=="DL")$GPP50, y = subset(df, Canal=="DL")$ER50, use = "complete.obs", method="kendall")


##### NEP and P/R #####
df$PRratio<-abs(df$GPP50/df$ER50)#Calculate production to respiration ratio
df$NEP<-df$GPP50-abs(df$ER50)#Calculate net ecosystem production

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
newbiomass<-data.frame(biomass$Date, biomass$Canal, biomass$mean_Biomass_per_Area, biomass$sd_Biomass_per_Area)
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











##### Plot biomass vs NEP #####
ggplot(data=wcb, aes(x=ave_Biomass_per_Area, y=aveWholeCanalGPP, col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=aveWholeCanalGPP-sdWholeCanalGPP, ymax=aveWholeCanalGPP+sdWholeCanalGPP), size=0.5)+
  geom_errorbarh(aes(xmin=ave_Biomass_per_Area-sd_Biomass_per_Area, xmax=ave_Biomass_per_Area+sd_Biomass_per_Area))+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("Biomass per area (g/", m^2,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  scale_y_continuous(limits = c(0,40))





##### Plot biomass vs GPP #####
ggplot(data=wcb, aes(x=ave_Biomass_per_Area, y=aveWholeCanalGPP, col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=aveWholeCanalGPP-sdWholeCanalGPP, ymax=aveWholeCanalGPP+sdWholeCanalGPP), size=0.5)+
  geom_errorbarh(aes(xmin=ave_Biomass_per_Area-sd_Biomass_per_Area, xmax=ave_Biomass_per_Area+sd_Biomass_per_Area))+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("Biomass per area (g/", m^2,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  scale_y_continuous(limits = c(0,40))

cor(wcb$ave_Biomass_per_Area, wcb$aveWholeCanalGPP, use = "complete.obs", method = "kendall")
summary(glm(aveWholeCanalGPP~Canal+Event+ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))
summary(glm(aveWholeCanalGPP~ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))


##### Plot biomass vs ER #####
ggplot(data=wcb, aes(x=ave_Biomass_per_Area, y=abs(aveWholeCanalER), col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=abs(aveWholeCanalER)-sdWholeCanalER, ymax=abs(aveWholeCanalER)+sdWholeCanalER), size=0.5)+
  geom_errorbarh(aes(xmin=ave_Biomass_per_Area-sd_Biomass_per_Area, xmax=ave_Biomass_per_Area+sd_Biomass_per_Area))+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("abs(ER) (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("Biomass per area (g/", m^2,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  scale_y_continuous(limits = c(0,40))

cor(wcb$ave_Biomass_per_Area, wcb$aveWholeCanalER, use = "complete.obs", method = "kendall")
summary(glm(abs(aveWholeCanalER)~Canal+Event+ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))
summary(glm(abs(aveWholeCanalER)~ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))


##### Plot sed %OM vs ER #####
ggplot(data=wcb, aes(x=aveSedPercentOM, y=abs(aveWholeCanalER), col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=abs(aveWholeCanalER)-sdWholeCanalER, ymax=abs(aveWholeCanalER)+sdWholeCanalER), size=0.5)+
  geom_errorbarh(aes(xmin=aveSedPercentOM-sdSedPercentOM, xmax=aveSedPercentOM+sdSedPercentOM))+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("abs(ER) (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("Sediment Percent (%) Organic Matter")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  scale_y_continuous(limits = c(0,40))

cor(wcb$aveSedPercentOM, wcb$aveWholeCanalER, use = "complete.obs", method = "kendall")
summary(glm(abs(aveWholeCanalER)~Canal+Event+aveSedPercentOM-1,data=wcb,family=Gamma(link="log")))
summary(glm(abs(aveWholeCanalER)~aveSedPercentOM-1,data=wcb,family=Gamma(link="log")))


##### Whole canal vs compartment metab #####
cor.test(wcb$avePlantGPP, wcb$aveWholeCanalGPP, use = "complete.obs", method = "kendall")
cor.test(wcb$avePlantER, wcb$aveWholeCanalER, use = "complete.obs", method = "kendall")
cor.test(wcb$aveSedER, wcb$aveWholeCanalER, use = "complete.obs", method = "kendall")


##### Plot light vs GPP #####
ggplot(data=wclt, aes(x=PAR, y=GPP50, col=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=GPP2.5, ymax=GPP97.5), size=0.5)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")), x="Average Daily Surface Light (PAR)")+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))

cor(wclt$PAR, wclt$GPP50, use = "complete.obs", method = "kendall")
summary(lm(GPP50 ~ PAR, data=subset(wclt, Canal=="UL")))
summary(lm(GPP50 ~ PAR, data=subset(wclt, Canal=="DL")))
summary(lm(GPP50 ~ PAR, data=subset(wclt, Canal=="A")))
summary(glm(GPP50~PAR-1,data=wclt,family=Gamma(link="log")))
summary(glm(GPP50~Canal+PAR-1,data=wclt,family=Gamma(link="log")))


##### Plot temp vs ER #####
ggplot(data=wclt, aes(x=Temp, y=-ER50, col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=-ER2.5, ymax=-ER97.5), size=0.5)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("|ER| (g ", O[2], " ", m^-2, " ", d^-1,")")), x="Average Daily Temperature (°C)")+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))

cor(wclt$Temp, wclt$ER50, use = "complete.obs", method = "kendall")
summary(lm(ER50 ~ Temp, data=subset(wclt, Canal=="UL")))
summary(lm(ER50 ~ Temp, data=subset(wclt, Canal=="DL")))
summary(lm(ER50 ~ Temp, data=subset(wclt, Canal=="A")))
summary(glm(abs(ER50)~Temp-1,data=wclt,family=Gamma(link="log")))
summary(glm(abs(ER50)~Canal+Temp-1,data=wclt,family=Gamma(link="log")))

#Save significant linear model and add to plot
lm_fit<-lm(ER50 ~ Temp, data=subset(wclt, Canal=="DL"))
predicted_df <- data.frame(ER_pred = predict(lm_fit, subset(wclt, Canal=="DL")), temp=subset(wclt, Canal=="DL")$Temp)

ggplot(data=wclt, aes(x=Temp, y=abs(ER50), col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=abs(ER2.5), ymax=abs(ER97.5)), size=0.5)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("|ER| (g ", O[2], " ", m^-2, " ", d^-1,")")), x="Average Daily Temperature (°C)")+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))+
  geom_line(color='#BF785E',data = predicted_df, aes(x=temp, y=abs(ER_pred)))


####################
## Figure 6 
####################

GPPlight_plot <- ggplot(data=wclt, aes(x=PAR, y=GPP50, col=Canal))+
  geom_point(size=3)+theme_bw()+
  geom_errorbar(aes(ymin=GPP2.5, ymax=GPP97.5), size=0.5)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")), 
       x=expression(paste("Mean Daily Surface PAR (μmol ", m^-2," ", s^-1,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14), legend.position = "bottom")

ERtemp_plot <- ggplot(data=wclt, aes(x=Temp, y=abs(ER50), col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=abs(ER2.5), ymax=abs(ER97.5)), size=0.5)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("|ER| (g ", O[2], " ", m^-2, " ", d^-1,")")), x="Average Daily Temperature (°C)")+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))+
  geom_line(color='#BF785E',data = predicted_df, aes(x=temp, y=abs(ER_pred)))


plot_grid(
  plot_grid(GPPlight_plot+theme(legend.position = "none"),
          ERtemp_plot+theme(legend.position = "none"),
          nrow = 1, ncol = 2, align = "hv", labels = c("A","B")),
  NULL, get_legend(GPPlight_plot),
  nrow = 3, rel_heights = c(1, -0.05, 0.15))




















