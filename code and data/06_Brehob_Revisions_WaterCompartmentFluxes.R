#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse","FSA"), require, character.only=T)

##################
## Import data
#################
df <- read.csv("Compartment metabolism calculations/Blanks_by_CanalEvent.csv")

# Split CanalEvent
df <- separate(
  data = df,
  col = CanalEvent,
  into = c("Canal", "Event"),
  sep = "_"
)

# Calculate GPP by adding abs(ER) to NEP
df$GPP <- df$Mean_Blank_Light + abs(df$Mean_Blank_Dark)

##############################
## Kruskal Wallis tests
##############################

#NEP
kruskal.test(Mean_Blank_Light ~ Canal, data = df) # No difference
kruskal.test(Mean_Blank_Light ~ Event, data = df) # p < 0.05

dunnTest(Mean_Blank_Light ~ Event, data = df, method="bonferroni") #1-3
# Figure
ggplot(df, aes(Event, Mean_Blank_Light))+
  geom_hline(yintercept = 0, linewidth = 0.5)+
  geom_boxplot()+geom_jitter(aes(color = Canal), width = 0.1, size = 3)+
  scale_color_manual(values = c("A" = "#6393A6","UL" = "#733B36", "DL" = "#BF785E"))+
  labs(y = expression('Water Compartment NEP (mg '*~O[2]~ L^-1~hr^-1*')'))+
  theme_bw()

#ER
kruskal.test(Mean_Blank_Dark ~ Canal, data = df) # NS
kruskal.test(Mean_Blank_Dark ~ Event, data = df) # NS
ggplot(df, aes(Event, Mean_Blank_Dark))+geom_boxplot()+geom_jitter()
  
#GPP
kruskal.test(GPP ~ Canal, data = df) # NS
kruskal.test(GPP ~ Event, data = df) # NS
ggplot(df, aes(Event, GPP))+geom_boxplot()+geom_jitter()








