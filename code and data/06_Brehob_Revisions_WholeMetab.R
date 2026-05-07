#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse","FSA"), require, character.only=T)

######################
## Import Data
######################
df <- read.csv("WholeCanalMetabolism_DailyValues.csv", header = T)

#Create NEP column
df$NEP50 <- df$GPP50 + df$ER50

#Make Canal a factor
df$Canal<-as.factor(df$Canal)

#Summarize mean values across all canals and all dates
df %>%
  summarise_at(.vars = c('GPP50','ER50'), .funs = mean)

#Kruskal Wallis test - Canal
kruskal.test(GPP50 ~ Canal, df) #NS
kruskal.test(ER50 ~ Canal, df) #p < 0.05
dunnTest(ER50 ~ Canal, df, method="bonferroni")
ggplot(df, aes(Canal, ER50))+geom_boxplot()+geom_jitter()
kruskal.test(NEP50 ~ Canal, df) #p < 0.05
dunnTest(NEP50 ~ Canal, df, method="bonferroni")
ggplot(df, aes(Canal, NEP50))+geom_boxplot()+geom_jitter()

#Kendall
cor(df$GPP50, df$ER50, method = "kendall")

###############
## Incorporate average GPP and ER data from prior event
###############
wcb <- read.csv("WholeCanal_Compartment_Metabolism_Biomass_SiteAverages.csv", header = T)

##### Plot biomass vs GPP #####
library(wql)
kendall_result <- mannKen(wcb$aveWholeCanalGPP)
sen_slope <- kendall_result$sen.slope
reference_x <- median(wcb$ave_Biomass_per_Area)
reference_y <- median(wcb$aveWholeCanalGPP, na.rm = TRUE)
# Calculate the intercept (b = y - mx)
intercept <- reference_y - sen_slope * reference_x


ggplot(data=wcb, aes(x=ave_Biomass_per_Area, y=aveWholeCanalGPP, col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=aveWholeCanalGPP-sdWholeCanalGPP, ymax=aveWholeCanalGPP+sdWholeCanalGPP), size=0.5)+
  geom_errorbarh(aes(xmin=ave_Biomass_per_Area-sd_Biomass_per_Area, xmax=ave_Biomass_per_Area+sd_Biomass_per_Area))+
  geom_abline(intercept = intercept, slope = sen_slope, color = "grey50", linetype = "dashed", size = 1)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("GPP (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("Biomass per area (g/", m^2,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  scale_y_continuous(limits = c(0,40))

cor(wcb$ave_Biomass_per_Area, wcb$aveWholeCanalGPP, use = "complete.obs", method = "kendall")
summary(glm(aveWholeCanalGPP~Canal+Event+ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))
summary(glm(aveWholeCanalGPP~ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))


##### Plot biomass vs ER #####
kendall_result <- mannKen(abs(wcb$aveWholeCanalER))
sen_slope <- kendall_result$sen.slope
reference_x <- median(wcb$ave_Biomass_per_Area)
reference_y <- median(abs(wcb$aveWholeCanalER), na.rm = TRUE)
# Calculate the intercept (b = y - mx)
intercept <- reference_y - sen_slope * reference_x

ggplot(data=wcb, aes(x=ave_Biomass_per_Area, y=abs(aveWholeCanalER), col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=abs(aveWholeCanalER)-sdWholeCanalER, ymax=abs(aveWholeCanalER)+sdWholeCanalER), size=0.5)+
  geom_errorbarh(aes(xmin=ave_Biomass_per_Area-sd_Biomass_per_Area, xmax=ave_Biomass_per_Area+sd_Biomass_per_Area))+
  geom_abline(intercept = intercept, slope = sen_slope, color = "grey50", linetype = "dashed", size = 1)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("|ER| (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("Biomass per area (g/", m^2,")")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  scale_y_continuous(limits = c(0,40))

cor(wcb$ave_Biomass_per_Area, wcb$aveWholeCanalER, use = "complete.obs", method = "kendall")
summary(glm(abs(aveWholeCanalER)~Canal+Event+ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))
summary(glm(abs(aveWholeCanalER)~ave_Biomass_per_Area-1,data=wcb,family=Gamma(link="log")))


##### Plot sed %OM vs ER #####
kendall_result <- mannKen(abs(wcb$aveWholeCanalER))
sen_slope <- kendall_result$sen.slope
reference_x <- median(wcb$aveSedPercentOM)
reference_y <- median(abs(wcb$aveWholeCanalER), na.rm = TRUE)
# Calculate the intercept (b = y - mx)
intercept <- reference_y - sen_slope * reference_x

ggplot(data=wcb, aes(x=aveSedPercentOM, y=abs(aveWholeCanalER), col=Canal))+
  geom_point(size=2.5)+theme_bw()+
  geom_errorbar(aes(ymin=abs(aveWholeCanalER)-sdWholeCanalER, ymax=abs(aveWholeCanalER)+sdWholeCanalER), size=0.5)+
  geom_errorbarh(aes(xmin=aveSedPercentOM-sdSedPercentOM, xmax=aveSedPercentOM+sdSedPercentOM))+
  geom_abline(intercept = intercept, slope = sen_slope, color = "grey50", linetype = "dashed", size = 1)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression(paste("|ER| (g ", O[2], " ", m^-2, " ", d^-1,")")), x=expression(paste("Sediment Percent (%) Organic Matter")))+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size=10))+
  scale_y_continuous(limits = c(0,40))

cor(wcb$aveSedPercentOM, wcb$aveWholeCanalER, use = "complete.obs", method = "kendall")
summary(glm(abs(aveWholeCanalER)~Canal+Event+aveSedPercentOM-1,data=wcb,family=Gamma(link="log")))
summary(glm(abs(aveWholeCanalER)~aveSedPercentOM-1,data=wcb,family=Gamma(link="log")))













