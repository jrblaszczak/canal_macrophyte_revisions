## Brehob revisions - Kruskal Wallis statistical tests on Environmental Data

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse","FSA"), require, character.only=T)

#Read in data
df<-read.csv("AllVar_byCanal_Event_Recompiled.csv", header = T)
View(df)

#Make Canal and Event factors
df$Event<-as.factor(df$Event)
df$Canal<-as.factor(df$Canal)

#Select non-biomass
colnames(df)
envt_var <- df %>% dplyr::select(Canal, Event,
                                 SpC..uS.cm.:TDN.ug.L.SW, PAR.30da:Light_ext_coeff)


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
kruskal_results_by_canal <- kruskal_all_columns_lapply(envt_var, Canal)

# Print all results
ldply(lapply(kruskal_results_by_canal, function(x) {
  cat("\nVariable:", x$data.name, "\n")
  cat("Chi-squared =", round(x$statistic, 3), 
      ", df =", x$parameter, 
      ", p-value =", format.pval(x$p.value, digits = 3), "\n")
}), data.frame)



# Kruskal Wallis test across Events
kruskal_results_by_event <- kruskal_all_columns_lapply(envt_var, Event)

# Print all results
ldply(lapply(kruskal_results_by_event, function(x) {
  cat("\nVariable:", x$data.name, "\n")
  cat("Chi-squared =", round(x$statistic, 3), 
      ", df =", x$parameter, 
      ", p-value =", format.pval(x$p.value, digits = 3), "\n")
}), data.frame)


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
KWresults_canal <- convert_kruskal_list_to_df(kruskal_results_by_canal)
KWresults_event <- convert_kruskal_list_to_df(kruskal_results_by_event)

write.csv(KWresults_canal, "EnvtVar_KWresults_Canal.csv")
write.csv(KWresults_event, "EnvtVar_KWresults_Event.csv")

#########################################################
### Use Dunn tests to explore post-hoc variation
#########################################################

#Significant Canal Differences
KWresults_canal[which(KWresults_canal$Significance == "*"),]$Variable
#Porewater TDC
kruskal.test(TDC.mg.L.PW ~ Canal, data = envt_var)
ggplot(envt_var, aes(Canal, TDC.mg.L.PW))+geom_boxplot()+geom_jitter()
dunnTest(TDC.mg.L.PW ~ Canal, data = envt_var, method="bonferroni")

#Significant Event Differences
KWresults_event[which(KWresults_event$Significance == "*"),]$Variable
#run Dunn across multiple columns
posthoc_Dunn <- function(x) {
  dunnTest(x ~ Event, data = envt_var, method="bonferroni")
}
apply(envt_var[,KWresults_event[which(KWresults_event$Significance == "*"),]$Variable], 2,
      function(y) posthoc_Dunn(y))

colnames(envt_var)
# TSS
ggplot(envt_var, aes(Event, TSS_mgmL))+geom_boxplot()+geom_jitter()
mean(envt_var[which(envt_var$Event == "3"),]$TSS_mgmL)/mean(envt_var[which(envt_var$Event == "1"),]$TSS_mgmL) #5-fold increase
envt_var$TSS_mgmL*1000
# Water temp
ggplot(envt_var, aes(Event, Temp.30da))+geom_boxplot()+geom_jitter()
envt_var[,c("Canal","Event","Temp.30da")] %>% group_by(Event) %>% summarise(across(Temp.30da, \(x) mean(x, na.rm = TRUE)))
# PM2.5
ggplot(envt_var, aes(Event, PM.30da))+geom_boxplot()+geom_jitter()
envt_var[,c("Canal","Event","PM.30da")] %>% group_by(Event) %>% summarise(across(PM.30da, \(x) mean(x, na.rm = TRUE)))
# Flow
ggplot(envt_var, aes(Event, Flow.cms.30da))+geom_boxplot()+geom_jitter()
# SpC
ggplot(envt_var, aes(Event, SpC..uS.cm.))+geom_boxplot()+geom_jitter()
# TDC
ggplot(envt_var, aes(Event, TDC.mg.L.SW))+geom_boxplot()+geom_jitter()
envt_var[,c("Canal","Event","TDC.mg.L.SW")] %>% group_by(Event) %>% summarise(across(TDC.mg.L.SW, \(x) mean(x, na.rm = TRUE)))
# o.Phosphate.ug.L.SW
ggplot(envt_var, aes(Event, o.Phosphate.ug.L.SW))+geom_boxplot()+geom_jitter()
envt_var[,c("Canal","Event","o.Phosphate.ug.L.SW")] %>% group_by(Event) %>% summarise(across(o.Phosphate.ug.L.SW, \(x) mean(x, na.rm = TRUE)))

## Create SI figure
sig_event_var <- envt_var %>% dplyr::select(Canal, Event,
                                            KWresults_event[which(KWresults_event$Significance == "*"),]$Variable,
                                            Flow.cms.30da)
sig_event_var <- sig_event_var[,!(names(sig_event_var) %in% "AQI.30da")]

sig_event_var <- sig_event_var %>%
  pivot_longer(cols = SpC..uS.cm.:Flow.cms.30da,
               names_to = "Variable",
               values_to = "Value")

ggplot(sig_event_var, aes(x = Event, y = Value))+
  geom_boxplot()+geom_jitter()+
  facet_wrap(~Variable, scales = "free", nrow = 4)+
  theme_bw()

