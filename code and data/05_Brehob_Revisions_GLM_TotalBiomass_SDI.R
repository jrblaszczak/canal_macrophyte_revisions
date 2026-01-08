## Brehob revisions - Kendall rank correlations for Total Biomass models

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse"), require, character.only=T)
library(AICcmodavg); library(sjPlot); library(glmtoolbox); library(performance)

## Import Data
df <- read.csv("MeanVariables_for_GLM_compiled.csv", header = T)
colnames(df)
selectedvar <- read.csv("Selected_Pred_Var_for_GLM.csv", header = T)

####################
## Biomass
####################

# Subset df for selected biomass model variables
pred_biomass_sub <- df %>% dplyr::select(Canal, Event, selectedvar[which(selectedvar$model == "Biomass_model"),]$Variables)
#missing values in NO3 causing issues so replaced with TDN
colnames(pred_biomass_sub)
#Make Canal and Event factors
pred_biomass_sub$Event<-as.factor(pred_biomass_sub$Event)
pred_biomass_sub$Canal<-as.factor(pred_biomass_sub$Canal)

#Center data
df_centered <- pred_biomass_sub
df_centered[,4:6] <- apply(df_centered[,4:6], 2, function(x) scale(x))

# Just Canal and Event
model0 <- glm(Biomass_per_Area~Canal+Event,data=pred_biomass_sub,family=Gamma(link="log"))
summary(model0)

## Different model variations
bio_model_var <- function(x){
  glm(x,data=df_centered,family=Gamma(link="log"))
}


bio_model_list <- list("TSS" = bio_model_var('Biomass_per_Area~Canal+Event+TSS_mgmL'),
                       "SW TDN" = bio_model_var('Biomass_per_Area~Canal+Event+TDN.ug.L.SW'),
                       "PW NH4" = bio_model_var('Biomass_per_Area~Canal+Event+Ammonium.mg.L.PW'),
                       "TSS_SW TDN" = bio_model_var('Biomass_per_Area~Canal+Event+TSS_mgmL+TDN.ug.L.SW'),
                       "TSS_PW NH4" = bio_model_var('Biomass_per_Area~Canal+Event+TSS_mgmL+Ammonium.mg.L.PW'),
                       "SW TDN_PW NH4" = bio_model_var('Biomass_per_Area~Canal+Event+TDN.ug.L.SW+Ammonium.mg.L.PW'))


# bio_model_list <- list("PAR" = bio_model_var('Biomass_per_Area~Canal+Event+PAR.30da'),
#                        "Temp" = bio_model_var('Biomass_per_Area~Canal+Event+Temp.30da'),
#                        "TSS" = bio_model_var('Biomass_per_Area~Canal+Event+TSS_mgmL'),
#                        "SW TDN" = bio_model_var('Biomass_per_Area~Canal+Event+TDN.ug.L.SW'),
#                        "PW NH4" = bio_model_var('Biomass_per_Area~Canal+Event+Ammonium.mg.L.PW'),
#                        "PAR_Temp" = bio_model_var('Biomass_per_Area~Canal+Event+PAR.30da+Temp.30da'),
#                        "PAR_TSS" = bio_model_var('Biomass_per_Area~Canal+Event+PAR.30da+TSS_mgmL'),
#                        "PAR_SW TDN" = bio_model_var('Biomass_per_Area~Canal+Event+PAR.30da+TDN.ug.L.SW'),
#                        "PAR_PW NH4" = bio_model_var('Biomass_per_Area~Canal+Event+PAR.30da+Ammonium.mg.L.PW'),
#                        "Temp_TSS" = bio_model_var('Biomass_per_Area~Canal+Event+Temp.30da+TSS_mgmL'),
#                        "Temp_SW TDN" = bio_model_var('Biomass_per_Area~Canal+Event+Temp.30da+TDN.ug.L.SW'),
#                        "Temp_PW NH4" = bio_model_var('Biomass_per_Area~Canal+Event+Temp.30da+Ammonium.mg.L.PW'),
#                        "TSS_SW TDN" = bio_model_var('Biomass_per_Area~Canal+Event+TSS_mgmL+TDN.ug.L.SW'),
#                        "TSS_PW NH4" = bio_model_var('Biomass_per_Area~Canal+Event+TSS_mgmL+Ammonium.mg.L.PW'))


bio_model_AIC <- aictab(cand.set = bio_model_list, modnames = names(bio_model_list), sort = TRUE, second.ord = FALSE)  #interpret: https://www.rdocumentation.org/packages/AICcmodavg/versions/2.3-4/topics/aictab
bio_model_AIC ## Best model TSS & NH4
write.csv(bio_model_AIC, "Biomass_GLM_AIC_Table_centered.csv")

summary(bio_model_list$`TSS_PW NH4`)

## Examine TSS & PW NH4 model
summary(bio_model_list$`TSS_PW NH4`)
r2(bio_model_list$`TSS_PW NH4`)
adjR2(bio_model_list$`TSS_PW NH4`) #0.99
plot_model(bio_model_list$`TSS_PW NH4`)

ggplot(pred_biomass_sub, aes(log(Ammonium.mg.L.PW), Biomass_per_Area, color = Canal))+geom_point(size = 3)

## Examine other models
lapply(bio_model_list, function(x) adjR2(x))
lapply(bio_model_list, function(x) summary(x))

####################
## SDI
####################

# Subset df for selected SDI model variables
pred_SDI_sub <- df %>% dplyr::select(Canal, Event, selectedvar[which(selectedvar$model == "SDI_model"),]$Variables)
colnames(pred_SDI_sub)

#Make Canal and Event factors
pred_SDI_sub$Event<-as.factor(pred_SDI_sub$Event)
pred_SDI_sub$Canal<-as.factor(pred_SDI_sub$Canal)
colnames(pred_SDI_sub)
#Center data
SDI_centered <- pred_SDI_sub
SDI_centered[,4:8] <- apply(SDI_centered[,4:8], 2, function(x) scale(x))

ggplot(SDI_centered, aes(TDC.mg.L.PW, SDI))+geom_point()

# Just Canal and Event
model0 <- glm(SDI~Canal+Event,data=SDI_centered,family=Gamma(link="log"))
summary(model0)

## Different model variations
SDI_model_var <- function(x){
  glm(x,data=SDI_centered,family=Gamma(link="log"))
}

SDI_model_list <- list("Temp" = SDI_model_var('SDI~Canal+Event+Temp.30da'),
                       "PW TDC" = SDI_model_var('SDI~Canal+Event+TDC.mg.L.PW'),
                       "PW TDN" = SDI_model_var('SDI~Canal+Event+TDN.ug.L.PW'),
                       "Sed OM" = SDI_model_var('SDI~Canal+Event+pct_OM'),
                       "BulkDens" = SDI_model_var('SDI~Canal+Event+Bulk_Density_g_mL'),
                       "Temp_PW TDC" = SDI_model_var('SDI~Canal+Event+Temp.30da+TDC.mg.L.PW'),
                       "Temp_PW TDN" = SDI_model_var('SDI~Canal+Event+Temp.30da+TDN.ug.L.PW'),
                       "Temp_Sed OM" = SDI_model_var('SDI~Canal+Event+Temp.30da+pct_OM'),
                       "Temp_BulkDens" = SDI_model_var('SDI~Canal+Event+Temp.30da+Bulk_Density_g_mL'),
                       "PW TDC_PW TDN" = SDI_model_var('SDI~Canal+Event+TDC.mg.L.PW+TDN.ug.L.PW'),
                       "PW TDC_Sed OM" = SDI_model_var('SDI~Canal+Event+TDC.mg.L.PW+pct_OM'),
                       "PW TDC_BulkDens" = SDI_model_var('SDI~Canal+Event+TDC.mg.L.PW+Bulk_Density_g_mL'),
                       "PW_TDN_Sed OM" = SDI_model_var('SDI~Canal+Event+TDN.ug.L.PW+pct_OM'),
                       "PW_TDN_BulkDens" = SDI_model_var('SDI~Canal+Event+TDN.ug.L.PW+Bulk_Density_g_mL'),
                       "Sed OM_BulkDens" = SDI_model_var('SDI~Canal+Event+pct_OM+Bulk_Density_g_mL'))

SDI_model_AIC <- aictab(cand.set = SDI_model_list, modnames = names(SDI_model_list), sort = TRUE, second.ord = FALSE)  #interpret: https://www.rdocumentation.org/packages/AICcmodavg/versions/2.3-4/topics/aictab
SDI_model_AIC
write.csv(SDI_model_AIC, "SDI_GLM_AIC_Table.csv")

## Examine best fit model
summary(SDI_model_list$`PW TDC_BulkDens`)
ggplot(pred_SDI_sub, aes(log(Bulk_Density_g_mL), SDI, color = Canal))+geom_point(size = 3)
ggplot(pred_SDI_sub, aes(log(TDC.mg.L.PW), SDI, color = Canal))+geom_point(size = 3)
plot_model(SDI_model_list$`PW TDC_BulkDens`)


