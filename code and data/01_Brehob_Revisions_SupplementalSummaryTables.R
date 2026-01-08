## Brehob revisions - Summary tables for SI

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)

###
#Read in Water Quality Data
###
WQ<-read.csv("Unmodified_WQCompiled_from_GitHub.csv", header = T)
WQ$Canal <- revalue(WQ$Canal, replace = c("A" = "A",
                                          "DRL" = "UL",
                                          "MSL" = "DL"))
#Replace the zero value NH4 (mg/L) with half the mdl (0.001 mg/L)
WQ[which(WQ$Ammonium.mg.L.SW == 0),]$Ammonium.mg.L.SW <- 0.001

#Remove Bromide, Fluoride, and Nitrite
cols_to_remove <- c("Fluoride.ppb.PW", "Nitrite.ppb.PW", "Bromide.ppb.PW",
                    "Fluoride.ppb.SW", "Nitrite.ppb.SW", "Bromide.ppb.SW")
WQ <- WQ[,!(names(WQ) %in% cols_to_remove)]
#Subset to only chemistry
colnames(WQ)
WQ <- WQ[,c(1,2,14,15,17:28)]
WQ$Canal <- as.factor(WQ$Canal); WQ$Event <- as.factor(WQ$Event)
## number of measurements per canal
WQ %>%
  group_by(Canal) %>%
  summarise_all(~ sum(!is.na(.)))


###
#Read in Biomass Data
###
B <- read.csv("Environmental_and_Macrophyte_AllObservations.csv")
Bio <- B[,c("Canal","Event","Biomass_per_Area")] # biomass has more measurements and therefore needs to be averaged separately
Type <- B[,c("Canal","Event","Curly.Leaf.Pondweed",
          "Ditchgrass","Elodea","Filamentous.Algae","Nitella", # ditchgrass renamed grass-like SAV
          "Milfoil","SDI")]
Bio$Canal <- as.factor(Bio$Canal); Bio$Event <- as.factor(Bio$Event)

Bio_mean <- Bio %>%
  group_by(Canal, Event) %>%
  summarise(across(Biomass_per_Area, mean, na.rm = TRUE))
## number of measurements per canal
Bio %>%
  group_by(Canal) %>%
  summarise_all(~ sum(!is.na(.)))


Type$Canal <- as.factor(Type$Canal); Type$Event <- as.factor(Type$Event)
Type_mean <- Type %>%
  group_by(Canal, Event) %>%
  summarise(across(Curly.Leaf.Pondweed:SDI, mean, na.rm = TRUE))
## number of measurements per canal
Type %>%
  group_by(Canal) %>%
  summarise_all(~ sum(!is.na(.)))

###
# Extract 30 day average data from B and average across repeating values
###

LTF_30da <- B[,c("Canal","Event","PAR.30da","AQI.30da","PM.30da","Temp.30da","Flow.cms.30da")]
LTF_30da <- LTF_30da %>%
  group_by(Canal, Event) %>%
  summarise(across(PAR.30da:Flow.cms.30da, mean, na.rm = TRUE))
LTF_30da$Canal <- as.factor(LTF_30da$Canal); LTF_30da$Event <- as.factor(LTF_30da$Event)

###
#Read in Sediment Data
###
sed<-read.csv("Sed_Data_Summary_from_GitHub.csv", header = T)
colnames(sed) <- c("Event","Canal","Treatment","Date","Transect","Bulk_Density_g_mL","pct_OM","sed_pH")
sed$Canal <- revalue(sed$Canal, replace = c("A" = "A",
                                            "DRL" = "UL",
                                            "MSL" = "DL"))
sed$Event<-as.factor(sed$Event); sed$Canal<-as.factor(sed$Canal)

sed_mean <- sed %>%
  group_by(Canal, Event) %>%
  summarise(across(Bulk_Density_g_mL:sed_pH, mean, na.rm = TRUE))

## number of measurements per canal
  sed %>%
  group_by(Canal) %>%
  summarise_all(~ sum(!is.na(.)))

###
#Read in TSS and Light Attenuation Data
###

L <- read.csv("TSS_Light_Attenuation.csv", header = T)
L <- L[,c("Event","Canal","TSS..mg.mL.",
          "Light.extinction.coefficient..for.z.in.cm.and.light.in.ln.lumens...")]
colnames(L) <- c("Event","Canal","TSS_mgmL","Light_ext_coeff")
L$Canal <- revalue(L$Canal, replace = c("A" = "A",
                                        "DRL" = "UL",
                                        "MSL" = "DL"))
L$Event<-as.factor(L$Event); L$Canal<-as.factor(L$Canal)

L_mean <- L %>%
  group_by(Canal, Event) %>%
  summarise(across(TSS_mgmL:Light_ext_coeff, mean, na.rm = TRUE))


############################
## Merge
############################
head(WQ); head(Bio_mean); head(Type_mean); head(LTF_30da); head(sed_mean); head(L_mean)

# Put all data frames into a list
df_list <- list(WQ, Bio_mean, Type_mean, LTF_30da, sed_mean, L_mean)

# Use reduce with a join function (e.g., full_join, inner_join)
merged_df_full <- df_list %>%
  reduce(full_join, by = c("Canal","Event"))

# Export for Kruskal-Wallis Envt Corr and Kendall correlations
write.csv(merged_df_full, "AllVar_byCanal_Event_Recompiled.csv")

##################################
## Summarize by Canal
##################################

mean_all <- merged_df_full[,c(1,3:34)] %>%
  group_by(Canal) %>%
  summarise(across(SpC..uS.cm.:Light_ext_coeff, mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Canal,
               names_to = "Variable",
               values_to = "Mean")

sd_all <- merged_df_full[,c(1,3:34)] %>%
  group_by(Canal) %>%
  summarise(across(SpC..uS.cm.:Light_ext_coeff, sd, na.rm = TRUE)) %>%
  pivot_longer(cols = -Canal,
               names_to = "Variable",
               values_to = "SD")


min_all <- merged_df_full[,c(1,3:34)] %>%
  group_by(Canal) %>%
  summarise(across(SpC..uS.cm.:Light_ext_coeff, min, na.rm = TRUE)) %>%
  pivot_longer(cols = -Canal,
               names_to = "Variable",
               values_to = "Min")


max_all <- merged_df_full[,c(1,3:34)] %>%
  group_by(Canal) %>%
  summarise(across(SpC..uS.cm.:Light_ext_coeff, max, na.rm = TRUE)) %>%
  pivot_longer(cols = -Canal,
               names_to = "Variable",
               values_to = "Max")

n_all <- merged_df_full[,c(1,3:34)] %>%
  group_by(Canal) %>%
  summarise_all(~ sum(!is.na(.))) %>%
  pivot_longer(cols = -Canal,
               names_to = "Variable",
               values_to = "n")

# Put all data frames into a list
summary_list <- list(mean_all, sd_all, min_all, max_all, n_all)

# Use reduce with a join function (e.g., full_join, inner_join)
merged_summary <- summary_list %>%
  reduce(full_join, by = c("Canal","Variable"))

write.csv(merged_summary, "All_Variables_Summary_Table.csv")




