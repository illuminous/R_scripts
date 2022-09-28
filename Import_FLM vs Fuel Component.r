
setwd("C:\\Documents and Settings\\alcorrow.DS\\Desktop\\FLM.FCCS.FIA Research Note\\Data\\")

FuelsWest<- read.table("FuelsWest.csv",header = TRUE, sep = ",", row.names= 1)
head(FuelsWest)
summary(FuelsWest)


# For all Keyed FLMs combined: 

plot(FuelsWest$FLM_plot, FuelsWest$LF2008_FLM_1x1, xlim = c(0,100), ylim= c(0,100), ylab = "FLMM 1x1 cell mode", xlab =" FLMF")
plot(FuelsWest$FLM_plot, FuelsWest$LF2008_FLM_5x5mode, xlim = c(0,100), ylim= c(0,100), ylab = "FLMM 5x5 cell mode", xlab =" FLMF")

FLM_fuelcomp <- par(mfrow=c(3,2), main = "FLM Fuel Components")

plot(FuelsWest$FLM_plot, FuelsWest$LITTER_BIOMASS, xlim = c(0,100), ylim= c(0,100), ylab = "Litter (tons per acre)", xlab =" FLMF")
plot(FuelsWest$FLM_plot, FuelsWest$DUFF_BIOMASS, xlim = c(0,100), ylim= c(0,100), ylab = "Duff (tons per acre)", xlab =" FLMF")
plot(FuelsWest$FLM_plot, FuelsWest$FWD_SM_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100), ylab = "1 Hr Fuels (tons per acre)", xlab =" FLMF")
plot(FuelsWest$FLM_plot, FuelsWest$FWD_MD_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100), ylab = "10 Hr Fuels (tons per acre)", xlab =" FLMF")
plot(FuelsWest$FLM_plot, FuelsWest$FWD_LG_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100), ylab = "100 Hr Fuels (tons per acre)", xlab =" FLMF")
plot(FuelsWest$FLM_plot, FuelsWest$CWD_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100), ylab = "1000 Hr Fuels (tons per acre)", xlab =" FLMF")
par(FLM_fuelcomp)


## **** use 'identify(x,y)' to id individual points on the plot.  
## Run plot line first, then identify line if they don't work together...  Do one plot at at time!!!  ******


plot(FuelsWest$FLM_plot, FuelsWest$FWD_LG_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100), ylab = "100 Hr Fuels (tons per acre)", xlab =" FLMF")
identify(FuelsWest$FLM_plot, FuelsWest$FWD_LG_DRYBIOT_AC)
