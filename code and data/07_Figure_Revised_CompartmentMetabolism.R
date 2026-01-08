## Redo of Figure 4 from barplots to boxplots

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

##################
## Import data
#################
df<-read.csv("Environmental_and_CompartmentMetabolism_AllObservations.csv")
#Make Canal and Event factors
df$Event<-as.factor(df$Event)
df$Canal<-as.factor(df$Canal)

## Rename events
df$Event_name <- revalue(as.factor(df$Event),
                                  c("1" = "Early",
                                    "2" = "Mid",
                                    "3" = "Late"))

## add in zero values for Late, Canal A given missing values
df[which(df$Canal == "A" & df$Event_name == "Late"),]$GPP.mg.per.g.per.hr <- 0
df[which(df$Canal == "A" & df$Event_name == "Late"),]$CR.mg.per.g.per.hr <- 0




##################
## Macrophyte GPP
##################
macrophytes <- df[which(df$Substrate == "Plant"),]

## order sites
macrophytes$Canal <- factor(macrophytes$Canal, levels = c('UL', 'DL', 'A'))

## Plot
macrophyte_gpp <-
  ggplot(macrophytes, aes(x = Event_name, y = GPP.mg.per.g.per.hr,
                        fill = Canal)) + 
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 6))+
  scale_fill_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression('Macrophyte GPP (mg '*~O[2]~ g^-1~ h^-1*')'),
       x = "Sampling Event")+
  theme_bw(base_size = 12)+
  theme(axis.text = element_text(size = 12))
macrophyte_gpp


macrophytes$NEP.mg.per.g.per.hr <- macrophytes$GPP.mg.per.g.per.hr - macrophytes$CR.mg.per.g.per.hr


macrophyte_cr <- ggplot(macrophytes, aes(x = Event_name, y = CR.mg.per.g.per.hr,
                        fill = Canal)) + 
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 6))+
  scale_fill_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression('Macrophyte |CR| (mg '*~O[2]~ g^-1~ h^-1*')'),
       x = "Sampling Event")+
  theme_bw(base_size = 12)+
  theme(axis.text = element_text(size = 12))
macrophyte_cr


macrophyte_nep <-
  ggplot(macrophytes, aes(x = Event_name, y = NEP.mg.per.g.per.hr,
                          fill = Canal)) + 
  geom_boxplot()+
  scale_fill_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression('Macrophyte NEP (mg '*~O[2]~ g^-1~ h^-1*')'),
       x = "Sampling Event")+
  geom_hline(yintercept = 0, size = 0.75)+
  theme_bw(base_size = 12)+
  theme(axis.text = element_text(size = 12))
macrophyte_nep


#############
## Sediment
##############
sediment <- df[which(df$Substrate == "Sed"),]
## order sites
sediment$Canal <- factor(sediment$Canal, levels = c('UL', 'DL', 'A'))


sediment_cr <- ggplot(sediment, aes(x = Event_name,
                                    y = CR.mg.per.g.per.hr,
                                    fill = Canal)) + 
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 6))+
  scale_fill_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y=expression('Sediment |CR| (mg '*~O[2]~ g^-1~ h^-1*')'),
       x = "Sampling Event")+
  theme_bw(base_size = 12)+
  theme(axis.text = element_text(size = 12))


##############################
## All together now
##############################
plot_grid(
plot_grid(macrophyte_gpp+theme(legend.position = "none",
                               axis.title.x = element_blank()),
          macrophyte_cr+theme(legend.position = "none",
                              axis.title.x = element_blank()),
          macrophyte_nep+theme(legend.position = "none",
                               axis.title.x = element_blank()),
          sediment_cr+theme(legend.position = "none",
                            axis.title.x = element_blank()),
          align = "hv",
          ncol = 2, nrow =2,labels = c("A","B","C","D")),
get_legend(macrophyte_gpp), nrow = 1, rel_widths = c(1,0.1))


#######################
## Biomass comparison
#######################
ggplot(macrophytes, aes(Biomass_per_Area, GPP.mg.per.g.per.hr,
                        color = Event_name, shape = Canal))+
  geom_point()
## nothing interesting


######################
## Kruskal tests
######################
macrophytes.nep <- na.omit(macrophytes %>% dplyr::select(Canal, Event, NEP.mg.per.g.per.hr))
kruskal.test(NEP.mg.per.g.per.hr ~ Canal, macrophytes.nep)
kruskal.test(NEP.mg.per.g.per.hr ~ Event, macrophytes.nep)

kruskal.test(CR.mg.per.g.per.hr ~ Canal, na.omit(sediment %>% dplyr::select(Canal, Event, CR.mg.per.g.per.hr))) #yes
kruskal.test(CR.mg.per.g.per.hr ~ Event, na.omit(sediment %>% dplyr::select(Canal, Event, CR.mg.per.g.per.hr))) #yes




















