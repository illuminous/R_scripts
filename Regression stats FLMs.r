
setwd("C:\\Documents and Settings\\alcorrow.DS\\Desktop\\FLM.FCCS.FIA Research Note\\Data\\")


library(MASS)  

FuelsWest<- read.table("FuelsWest.csv",header = TRUE, sep = ",", row.names= 1)
head(FuelsWest)
summary(FuelsWest)
FuelsWest


# For all Keyed FLMs combined: 

lm.1x1 <- lm(FuelsWest$FLM_plot~ FuelsWest$LF2008_FLM_1x1)
summary(lm.1x1)

lm.1x1 <- lm(FuelsWest$FLM_plot, FuelsWest$LF2008_FLM_5x5mode)
summary(lm.5x5)

lm.litter <- lm(FuelsWest$FLM_plot ~ FuelsWest$LITTER_BIOMASS)
summary(lm.litter)

lm.duff <- lm(FuelsWest$FLM_plot~ FuelsWest$DUFF_BIOMASS)
summary(lm.duff)

lm.1hr <- lm(FuelsWest$FLM_plot~ FuelsWest$FWD_SM_DRYBIOT_AC)
summary(lm.1hr)

lm.10hr <- lm(FuelsWest$FLM_plot~ FuelsWest$FWD_MD_DRYBIOT_AC)
summary(lm.10hr)

lm.100hr <- lm(FuelsWest$FLM_plot~ FuelsWest$FWD_LG_DRYBIOT_AC)
summary(lm.100hr)

lm.1000hr <- lm(FuelsWest$FLM_plot~ FuelsWest$CWD_DRYBIOT_AC)
summary(lm.1000hr)

########## Below is work in progress

setwd("C:\\Documents and Settings\\alcorrow.DS\\Desktop\\FLM.FCCS.FIA Research Note\\Data\\")

FuelsWest<- read.table("FuelsWest.csv",header = TRUE, sep = ",", row.names= 1)
head(FuelsWest)
summary(FuelsWest)

lm.11 ->lm((FuelsWest$FLM_plot[FuelsWest$FLM_plot==11]~(FuelsWest$DUFF_BIOMASS[FuelsWest$FLM_plot==11]))

summary(lm.11)

lm(FuelsWest$FLM_plot~ FuelsWest$FWD_SM_DRYBIOT_AC)