## Brehob revisions - TSS Figure

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

df <- read.csv("TSS_Light_Attenuation.csv", header = T)
colnames(df)

sub <- df[,c("Event","Canal","TSS..mg.mL.",
             "Light.extinction.coefficient..for.z.in.cm.and.light.in.ln.lumens...")]
colnames(sub) <- c("Event","Canal","TSS_mgmL","Light_ext_coeff")

## Rename event, canals and add colors
sub$Event_name <- revalue(as.factor(sub$Event),
                                     c("1" = "Early Summer",
                                       "2" = "Mid Summer",
                                       "3" = "Late Summer"))
sub$Canal_name <- revalue(sub$Canal, replace = c("A" = "A",
                                                 "DRL" = "UL",
                                                 "MSL" = "DL"))

sub$Canal_name <- factor(sub$Canal_name, levels = c('UL', 'DL', 'A'))#creating fill order so that it is in correct sampling order


## Plot
TSSfig <- ggplot(sub, aes(x = Canal_name, y = TSS_mgmL, fill = Canal_name))+
  geom_boxplot()+
  scale_fill_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  geom_jitter(color="black", size=3, alpha=0.9, width = 0.05)+
  guides(fill = guide_legend("Canal"))+
  labs(y = "TSS (mg/mL)", x = "Canal")+
  theme_bw(base_size = 12)
TSSfig

LEfig <- ggplot(sub, aes(TSS_mgmL, Light_ext_coeff))+
  geom_point(aes(color = Canal_name, shape = as.factor(Event_name)),
             size = 3)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  geom_smooth(method = "lm", color = "black")+
  guides(color = guide_legend("Canal"),
         shape = guide_legend("Event"))+
  labs(x = "TSS (mg/mL)", y = "Light extinction coefficient (lumens/cm)")+
  theme_bw(base_size = 12)
LEfig



plot_grid(TSSfig+theme(legend.position = "blank"),
          LEfig+theme(legend.position = "blank"),
          ncol = 2, labels = c("a","b"), align = "hv")
# can't get separate legend to work
#get_plot_component(LEfig, "guide-box", return_all = TRUE),

cor.test(sub$TSS_mgmL, sub$Light_ext_coeff)
summary(aov(TSS_mgmL ~ Canal + Event, data = sub))
summary(aov(TSS_mgmL ~ Canal, data = sub))

kruskal.test(sub$TSS_mgmL ~ sub$Canal)

