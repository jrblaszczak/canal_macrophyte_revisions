## Brehob revisions - Kendall Correlation Matrices - Variable selection

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse","FSA"), require, character.only=T)

#Read in data
df<-read.csv("AllVar_byCanal_Event_Recompiled.csv", header = T)
#View(df)
#Make Canal and Event factors
df$Event<-as.factor(df$Event); df$Canal<-as.factor(df$Canal)

#Import GPP and ER data
wcb<-read.csv("WholeCanal_Compartment_Metabolism_Biomass_SiteAverages.csv")
colnames(wcb)
wc <- wcb[,c("Canal","Event","aveWholeCanalGPP","aveWholeCanalER")]
colnames(wc) <- c("Canal","Event","WholeCanalGPP","WholeCanalER")
#Make Canal and Event factors
wc$Event<-as.factor(wc$Event); wc$Canal<-as.factor(wc$Canal)

#Merge with df
df <- merge(df, wc, by = c("Canal","Event"))

#Remove canal A event 3 because there is unexplained zero data during this event
df<-subset(df, Canal!="A" | Event!="3")

write.csv(df, "MeanVariables_for_GLM_compiled.csv")

################################################
## Predictor Variable Kendall Correlations
################################################
colnames(df)
# 4 categories:
# (1) 30 day antecedent conditions (light, flow, PM2.5, water temp)
# (2) Light attenuation in the water column
# (3) Surface water chemistry
# (4) Extracted porewater chemistry
# (5) Sediment properties

Cat1_ant <- df %>% dplyr::select(Canal, Event, Biomass_per_Area, SDI, WholeCanalGPP, WholeCanalER, PAR.30da,PM.30da:Flow.cms.30da) # remove AQI since not quantitative
Cat2_atten <- df %>% dplyr::select(Canal, Event, Biomass_per_Area, SDI, WholeCanalGPP, WholeCanalER, TSS_mgmL, Light_ext_coeff)
Cat3_SWchem <- df %>% dplyr::select(Canal, Event, Biomass_per_Area, SDI, WholeCanalGPP, WholeCanalER, SpC..uS.cm.:pH, o.Phosphate.ug.L.SW:TDN.ug.L.SW)
Cat4_PWchem <- df %>% dplyr::select(Canal, Event, Biomass_per_Area, SDI, WholeCanalGPP, WholeCanalER, o.Phosphate.ug.L.PW:TDN.ug.L.PW,sed_pH)
Cat5_sed <- df %>% dplyr::select(Canal, Event, Biomass_per_Area, SDI, WholeCanalGPP, WholeCanalER, Bulk_Density_g_mL, pct_OM)

Cat_list <- list(Cat1_ant, Cat2_atten, Cat3_SWchem, Cat4_PWchem, Cat5_sed)

# Function to handle missing values and calculate Kendall correlation
calculate_kendall_cor <- function(dat) {
  # Remove columns Canal and Event
  data <- dat %>%
    select(-Canal, -Event)
  
  # Create an empty matrix for correlations
  n_vars <- ncol(data)
  cor_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(data)
  
  # Calculate pairwise Kendall correlations (handles missing values pairwise)
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i <= j) {
        # Extract the two variables
        x <- data[[i]]
        y <- data[[j]]
        
        # Remove rows where either variable is NA
        complete_cases <- complete.cases(x, y)
        x_complete <- x[complete_cases]
        y_complete <- y[complete_cases]
        
        # Only calculate correlation if we have enough observations
        if (length(x_complete) >= 3 && length(y_complete) >= 3) {
          cor_matrix[i, j] <- cor(x_complete, y_complete, 
                                  method = "kendall", use = "complete.obs")
          cor_matrix[j, i] <- cor_matrix[i, j]  # Symmetric matrix
        }
      }
    }
  }
  
  return(cor_matrix)
}

# Calculate Kendall correlation matrix
Kcorr_list <- lapply(Cat_list, function(x) calculate_kendall_cor(x))
# Print matrices
lapply(Kcorr_list, function(y) print(y))

## Export
write.csv(as.data.frame(Kcorr_list[1]),"KendallcorrMatrix_Cat1.csv")
write.csv(as.data.frame(Kcorr_list[2]),"KendallcorrMatrix_Cat2.csv")
write.csv(as.data.frame(Kcorr_list[3]),"KendallcorrMatrix_Cat3.csv")
write.csv(as.data.frame(Kcorr_list[4]),"KendallcorrMatrix_Cat4.csv")
write.csv(as.data.frame(Kcorr_list[5]),"KendallcorrMatrix_Cat5.csv")

## Select only up to two per category
## Subset variables to include based on Kendall Corr Matices (tau > |0.3| and not highly corr)

# For Total Biomass
bio_pred <- df %>% dplyr::select(Canal, Event, Biomass_per_Area,
                                 #PAR.30da, #Cat 1 - need to remove because tau > 0.7 with TSS
                                 TSS_mgmL, #Cat 2
                                 TDN.ug.L.SW, #Cat 3 - NA values in NO3 caused problems in GLM; need to remove Temp then
                                 Ammonium.mg.L.PW) #Cat 4 (no Cat 5)
ggplot(bio_pred, aes(log(Ammonium.mg.L.PW), Biomass_per_Area, color = Canal))+geom_point(size = 3)
#Recheck Correlation matrix
calculate_kendall_cor(bio_pred)
# Export
write.csv(calculate_kendall_cor(bio_pred), "Biomodel_Var_Selected.csv")

# For SDI
SDI_pred <- df %>% dplyr::select(Canal, Event, SDI,
                                 Temp.30da, #Cat 1 (no Cat 2)
                                 #TDN.ug.L.SW, #Cat 3 - NH4 missing values caused problems in GLM but priortizing temp over this
                                 TDC.mg.L.PW, TDN.ug.L.PW, #Cat 4
                                 pct_OM, Bulk_Density_g_mL) #Cat 5

ggplot(SDI_pred, aes(SDI, TDN.ug.L.PW, color = Canal))+geom_point(size = 3)
#Recheck Correlation matrix
calculate_kendall_cor(SDI_pred)
# Export
write.csv(calculate_kendall_cor(SDI_pred), "SDImodel_Var_Selected.csv")

####################################
## Compile names of pred variables
####################################
biomass_vars <- as.data.frame(colnames(calculate_kendall_cor(bio_pred))); biomass_vars$model <- "Biomass_model";colnames(biomass_vars)[1] <- "Variables"
SDI_vars <- as.data.frame(colnames(calculate_kendall_cor(SDI_pred))); SDI_vars$model <- "SDI_model";colnames(SDI_vars)[1] <- "Variables"


selectedvar <- rbind(biomass_vars,SDI_vars)
write.csv(selectedvar, "Selected_Pred_Var_for_GLM.csv")



