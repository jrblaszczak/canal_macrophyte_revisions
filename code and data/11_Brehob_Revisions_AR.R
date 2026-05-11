## Autotrophic respiration
## Only within DL which had enough data

#######################
## Import packages
#######################
lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse","quantreg"), require, character.only=T)

######################
## Import Data
######################
df <- read.csv("WholeCanalMetabolism_DailyValues.csv", header = T)
# only do DL which has the most data
DL <- df[which(df$Canal == "DL"),]

## Check basic requirements:
ggplot(DL, aes(GPP50, abs(ER50)))+
  geom_point(size = 3)+theme_bw()

# 1. CV of GPP must be > 0.5
sd(DL$GPP50)/mean(DL$GPP50) # 0.53
# 2. Correlation between HR and GPP must be < 0.3, otherwise biased - check further down

## Fit quantile regression
qr_fit <- rq(formula = abs(ER50) ~ GPP50,
             data = DL, tau = 0.1) #tau is the inverse of the 90th quantile for neg ER values
summary(qr_fit, se = "boot", R = 500) ## ARf slope = 0.58478

# AR = ARf*GPP
DL$AR <- DL$GPP50*coef(qr_fit)["GPP50"]
# HR = ER - AR
DL$HR = DL$ER50 + DL$AR
# check HR and GPP correlation

cor.test(DL$HR,DL$GPP50) # correlation coefficient > 0.3














