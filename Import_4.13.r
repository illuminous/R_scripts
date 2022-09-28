library(RODBC)
setwd("C:\\Documents and Settings\\alcorrow.DS\\Desktop\\FLM.FCCS.FIA Research Note\\Data\\")

FuelsWest<- read.table("FuelsWest.csv",header = TRUE, sep = ",", row.names= 1)

head(FuelsWest)

summary(FuelsWest)




# For all Keyed FLMs combined: 

plot(FuelsWest$FLM_plot, FuelsWest$LF2008_FLM_1x1, xlim = c(0,100), ylim= c(0,100))

plot(FuelsWest$FLM_plot, FuelsWest$LF2008_FLM_5x5mode, xlim = c(0,100), ylim= c(0,100))

plot(FuelsWest$FLM_plot, FuelsWest$LITTER_BIOMASS, xlim = c(0,100), ylim= c(0,100))

plot(FuelsWest$FLM_plot, FuelsWest$DUFF_BIOMASS, xlim = c(0,100), ylim= c(0,100))

plot(FuelsWest$FLM_plot, FuelsWest$DUFF_BIOMASS, xlim = c(0,100), ylim= c(0,100))
plot(FuelsWest$FLM_plot, FuelsWest$FWD_LG_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100))
plot(FuelsWest$FLM_plot, FuelsWest$FWD_MD_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100))
plot(FuelsWest$FLM_plot, FuelsWest$FWD_SM_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100))
plot(FuelsWest$FLM_plot, FuelsWest$CWD_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100))







identify()
