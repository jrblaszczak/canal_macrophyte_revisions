## Brehob revisions - Results paragraphs
## code modified from "SeasonalSiteVariation_Macrophytes_EnvironmentalConditions.R" in USBR data release

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse", "FSA"), require, character.only=T)

#Read in data
dat<-read.csv("Environmental_and_Macrophyte_AllObservations.csv")

#Make Canal and Event factors
dat$Event<-as.factor(dat$Event)
dat$Canal<-as.factor(dat$Canal)

#Remove canal A event 3 because there is unexplained zero data during this event
dat<-subset(dat, Canal!="A" | Event!="3")

# subset to only biomass and macrophytes to avoid repeated values
bio_df <- dat[,c("Canal","Event","Transect","Biomass_per_Area")]
mac_df <- dat[,c("Canal","Event","Transect","Biomass_per_Area","Curly.Leaf.Pondweed",
                "Ditchgrass","Elodea","Filamentous.Algae","Nitella", # ditchgrass renamed grass-like SAV
                "Milfoil","SDI")]
mac_df <- na.omit(mac_df) # omit samples that were not sorted

##### Biomass per Area #####
#Used Kruskal Wallis and posthoc Dunn tests for biomass due to violation of assumptions of ANOVA.

# Function to use lapply across columns
kruskal_all_columns_lapply <- function(data, group_var) {
  group_var <- as.character(substitute(group_var))
  
  # Get numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, group_var)
  
  # Apply Kruskal-Wallis test to each column
  results <- lapply(numeric_cols, function(col) {
    formula <- as.formula(paste(col, "~", group_var))
    kruskal.test(formula, data = data)
  })
  
  # Name the results
  names(results) <- numeric_cols
  
  return(results)
}

# Kruskal Wallis test across Canals
kruskal_results_by_canal_bio <- kruskal_all_columns_lapply(bio_df, Canal)
kruskal_results_by_canal_mac <- kruskal_all_columns_lapply(mac_df, Canal)

# Print all results
ldply(lapply(kruskal_results_by_canal_bio, function(x) {
  cat("\nVariable:", x$data.name, "\n")
  cat("Chi-squared =", round(x$statistic, 3), 
      ", df =", x$parameter, 
      ", p-value =", format.pval(x$p.value, digits = 3), "\n")
}), data.frame) #NS
ldply(lapply(kruskal_results_by_canal_mac, function(x) {
  cat("\nVariable:", x$data.name, "\n")
  cat("Chi-squared =", round(x$statistic, 3), 
      ", df =", x$parameter, 
      ", p-value =", format.pval(x$p.value, digits = 3), "\n")
}), data.frame) #Only Elodea, Algae, and SDI

# Kruskal Wallis test across Events
kruskal_results_by_event_bio <- kruskal_all_columns_lapply(bio_df, Event)
kruskal_results_by_event_mac <- kruskal_all_columns_lapply(mac_df, Event)

# Print all results
ldply(lapply(kruskal_results_by_event_bio, function(x) {
  cat("\nVariable:", x$data.name, "\n")
  cat("Chi-squared =", round(x$statistic, 3), 
      ", df =", x$parameter, 
      ", p-value =", format.pval(x$p.value, digits = 3), "\n")
}), data.frame) #Biomass p < 0.01
ldply(lapply(kruskal_results_by_event_mac, function(x) {
  cat("\nVariable:", x$data.name, "\n")
  cat("Chi-squared =", round(x$statistic, 3), 
      ", df =", x$parameter, 
      ", p-value =", format.pval(x$p.value, digits = 3), "\n")
}), data.frame) #Grass-like SAV, Elodea, Nitella, Milfoil, SDI


## Convert to data frame
# Function to convert the list output to a data frame
convert_kruskal_list_to_df <- function(kruskal_list) {
  # Extract information from each test result
  results_df <- data.frame(
    Variable = names(kruskal_list),
    Chi_Squared = sapply(kruskal_list, function(x) round(x$statistic, 4)),
    df = sapply(kruskal_list, function(x) x$parameter),
    p_value = sapply(kruskal_list, function(x) x$p.value),
    Method = sapply(kruskal_list, function(x) x$method),
    stringsAsFactors = FALSE
  )
  
  # Add significance levels
  results_df$Significance <- ifelse(
    results_df$p_value < 0.001, "***",
    ifelse(results_df$p_value < 0.01, "**",
           ifelse(results_df$p_value < 0.05, "*", "ns"))
  )
  
  # Format p-values for readability
  results_df$p_value_formatted <- format.pval(
    results_df$p_value, 
    digits = 3, 
    eps = 0.001
  )
  
  # Add adjusted p-values
  results_df$p_adj_BH <- p.adjust(results_df$p_value, method = "BH")
  
  # Reorder columns for better readability
  results_df <- results_df[, c("Variable", "Chi_Squared", "df",
                               "p_value_formatted", "p_adj_BH",
                               "Significance")]
  
  # Order by p-value (most significant first)
  results_df <- results_df[order(results_df$p_value), ]
  
  # Reset row names
  rownames(results_df) <- NULL
  
  return(results_df)
}


## Create data frames 
write.csv(convert_kruskal_list_to_df(kruskal_results_by_canal_bio), "BioVar_KWresults_canal.csv")
write.csv(convert_kruskal_list_to_df(kruskal_results_by_canal_mac), "MacVar_KWresults_canal.csv")
write.csv(convert_kruskal_list_to_df(kruskal_results_by_event_bio), "BioVar_KWresults_event.csv")
write.csv(convert_kruskal_list_to_df(kruskal_results_by_event_mac), "MacVar_KWresults_event.csv")

##########################
## Posthoc Dunn tests
##########################
#run Dunn across multiple columns
posthoc_Dunn_event <- function(x) {
  dunnTest(x ~ Event, data = mac_df, method="bonferroni")
}
posthoc_Dunn_canal <- function(x) {
  dunnTest(x ~ Canal, data = mac_df, method="bonferroni")
}
#by canal
MC <- convert_kruskal_list_to_df(kruskal_results_by_canal_mac)
apply(mac_df[,MC[which(MC$p_adj_BH < 0.05),]$Variable], 2,function(y) posthoc_Dunn_canal(y))
dunnTest(Filamentous.Algae ~ Canal, data = mac_df, method = "bonferroni")
#by event
ME <- convert_kruskal_list_to_df(kruskal_results_by_event_mac)
apply(mac_df[,ME[which(ME$p_adj_BH < 0.05),]$Variable], 2,function(y) posthoc_Dunn_event(y))
dunnTest(Ditchgrass ~ Event, data = mac_df, method = "bonferroni")

## Seasonal variation
#All canals
kruskal.test(Biomass_per_Area ~ Event, data = bio_df)
dunnTest(Biomass_per_Area ~ Event, data = bio_df, method="bonferroni")
ggplot(bio_df, aes(Event, Biomass_per_Area))+geom_boxplot()+geom_jitter()
#A-Line
kruskal.test(Biomass_per_Area ~ Event, data = subset(bio_df, Canal=="A"))#Yes
dunnTest(subset(bio_df, Canal=="A")$Biomass_per_Area, subset(bio_df, Canal=="A")$Event, method="bonferroni")
#UL
kruskal.test(Biomass_per_Area ~ Event, data = subset(bio_df, Canal=="UL"))#Yes
dunnTest(subset(bio_df, Canal=="UL")$Biomass_per_Area, subset(bio_df, Canal=="UL")$Event, method="bonferroni")
#DL
kruskal.test(Biomass_per_Area ~ Event, data = subset(bio_df, Canal=="DL"))#Yes
dunnTest(subset(bio_df, Canal=="DL")$Biomass_per_Area, subset(bio_df, Canal=="DL")$Event, method="bonferroni")



##### Autotrophic Biomass (Macrophytes and Algae) Diversity #####

#Percent filamentous algae
colnames(mac_df)
mac_df$Biomass_Sum <- rowSums(mac_df[,5:10])
mac_df$pct_algae <- mac_df$Filamentous.Algae/mac_df$Biomass_Sum
min(mac_df$pct_algae,na.rm = TRUE); max(mac_df$pct_algae,na.rm = TRUE)
mean(mac_df$pct_algae,na.rm = TRUE)

kruskal.test(pct_algae ~ Event, data = mac_df) # NS
kruskal.test(pct_algae ~ Canal, data = mac_df) # p < 0.01
dunnTest(mac_df$pct_algae, mac_df$Canal, method="bonferroni")
ggplot(mac_df, aes(y = pct_algae, x = Canal))+geom_boxplot()+
  theme_bw()

## Seasonal variation
#All canals
kruskal.test(SDI ~ Event, data = mac_df)# p < 0.05
#A-Line
kruskal.test(SDI ~ Event, data = subset(mac_df, Canal=="A"))#No
#UL
kruskal.test(SDI ~ Event, data = subset(mac_df, Canal=="UL"))#Yes
#DL
kruskal.test(SDI ~ Event, data = subset(mac_df, Canal=="DL"))#Yes









########################
## Old code below
########################





##Site variation
#All canals
kruskal.test(SDI ~ Canal, data = mac_df)#Yes
pairwise.wilcox.test(mac_df$SDI, mac_df$Canal, p.adjust.method = "BH", exact=F)
#Event 1
kruskal.test(SDI ~ Canal, data = subset(mac_df, Event=="1"))#Yes
pairwise.wilcox.test(subset(mac_df, Event=="1")$SDI, subset(mac_df, Event=="1")$Canal, p.adjust.method = "BH", exact=F)
#Event 2
kruskal.test(SDI ~ Canal, data = subset(mac_df, Event=="2"))#Yes
pairwise.wilcox.test(subset(mac_df, Event=="2")$SDI, subset(mac_df, Event=="2")$Canal, p.adjust.method = "BH", exact=F)
#Event 3
kruskal.test(SDI ~ Canal, data = subset(mac_df, Event=="3"))#Yes
pairwise.wilcox.test(subset(mac_df, Event=="3")$SDI, subset(mac_df, Event=="3")$Canal, p.adjust.method = "BH", exact=F)


##### Correlation between macrophyte density and diversity #####
cor.test(df$Biomass_per_Area, df$SDI, use = "complete.obs", method = "kendall", exact=F)


##### Within and among site variability in macrophyte density and diversity #####
##Biomass
within_variance <- bio_df %>% group_by(Canal, Event) %>%
  summarise(
    cov=sd(Biomass_per_Area)/mean(Biomass_per_Area)
  )
range(within_variance$cov, na.rm=TRUE)

dens_aves <- bio_df %>% group_by(Canal, Event) %>%
  summarise(
    mean_Biomass_per_Area=mean(Biomass_per_Area, na.rm=T)
  )

among_variance <- dens_aves %>% group_by(Event) %>%
  summarise(
    cov=sd(mean_Biomass_per_Area)/mean(mean_Biomass_per_Area)
  )

sd(dens_aves$mean_Biomass_per_Area)/mean(dens_aves$mean_Biomass_per_Area)

##Diversity
SDI_within_variance <- mac_df %>% group_by(Canal, Event) %>%
  summarise(
    cov=sd(SDI, na.rm = T)/mean(SDI, na.rm=T)
  )
range(SDI_within_variance$cov, na.rm=TRUE)

SDIaves <- mac_df %>% group_by(Canal, Event) %>%
  summarise(
    meansdi=mean(SDI, na.rm=T)
  )

SDI_among_variance <- SDIaves %>% group_by(Event) %>%
  summarise(
    cov=sd(meansdi)/mean(meansdi)
  )

sd(SDIaves$meansdi)/mean(SDIaves$meansdi)


##### Curly Leaf Pondweed #####
#Visualize CLPW presence
ggplot(mac_df, aes(x=Event,y=Curly.Leaf.Pondweed, col=Canal))+geom_point()+
  scale_color_manual(breaks = c("UL", "DL", "A"),
                     values=c("#F8766D", "#00BA38", "#83b0fc"))+
  labs(x="Event", y="CLPW bpa")
##Seasonal variation
kruskal.test(Curly.Leaf.Pondweed ~ Event, data = mac_df)#No
##Site variation
kruskal.test(Curly.Leaf.Pondweed ~ Canal, data = mac_df)#No


##### Elodea #####
#Visualize Elodea presence
ggplot(mac_df, aes(x=Event,y=Elodea, col=Canal))+geom_point()+
  scale_color_manual(breaks = c("UL", "DL", "A"),
                     values=c("#F8766D", "#00BA38", "#83b0fc"))+
  labs(x="Event", y="Elodea bpa")
##Seasonal variation
kruskal.test(Elodea ~ Event, data = mac_df)#Yes
##Site variation
kruskal.test(Elodea ~ Canal, data = mac_df)#Yes


##### Filamentous Algae #####
#Visualize Fil Algae presence
ggplot(mac_df, aes(x=Event,y=Filamentous.Algae, col=Canal))+geom_point()+
  scale_color_manual(breaks = c("UL", "DL", "A"),
                     values=c("#F8766D", "#00BA38", "#83b0fc"))+
  labs(x="Event", y="Fil Algae bpa")
##Seasonal variation
kruskal.test(Filamentous.Algae ~ Event, data = mac_df)#No
##Site variation
kruskal.test(Filamentous.Algae ~ Canal, data = mac_df)#Yes


##### Nitella #####
#Visualize Nitella presence
ggplot(mac_df, aes(x=Event,y=Nitella, col=Canal))+geom_point()+
  scale_color_manual(breaks = c("UL", "DL", "A"),
                     values=c("#F8766D", "#00BA38", "#83b0fc"))+
  labs(x="Event", y="Nitella bpa")
##Seasonal variation
kruskal.test(Nitella ~ Event, data = mac_df)#Yes
##Site variation
kruskal.test(Nitella ~ Canal, data = mac_df)#No


##### Milfoil #####
#Visualize Milfoil presence
ggplot(mac_df, aes(x=Event,y=Milfoil, col=Canal))+geom_point()+
  scale_color_manual(breaks = c("UL", "DL", "A"),
                     values=c("#F8766D", "#00BA38", "#83b0fc"))+
  labs(x="Event", y="Milfoil bpa")
##Seasonal variation
kruskal.test(Milfoil ~ Event, data = mac_df)#Yes
##Site variation
kruskal.test(Milfoil ~ Canal, data = mac_df)#No



