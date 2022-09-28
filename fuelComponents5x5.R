
setwd("C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\")

FuelsWest<- read.table("Fuel_load_west_analysis_set_5x5.csv",header = TRUE, sep = ",", row.names= 1)
head(FuelsWest)
summary(FuelsWest)


# For all Keyed FLMs combined: 

plot(FuelsWest$FLM_plot, FuelsWest$LF2008_FLM_1x1, xlim = c(0,100), ylim= c(0,100), ylab = "FLMM 1x1 cell mode", xlab =" FLMF")
plot(FuelsWest$FLM_plot, FuelsWest$LF2008_FLM_5x5mode, xlim = c(0,100), ylim= c(0,100), ylab = "FLMM 5x5 cell mode", xlab =" FLMF")

FLM_fuelcomp <- par(mfrow=c(3,2), main = "FLM Fuel Components")

litter<-plot(FuelsWest$FIA_Litter.kg.m.2, FuelsWest$fivex_Litter.kg.m.2, xlim = c(0,15), ylim= c(0,5), ylab = expression(paste("LANDFIRE Litter median kg/m" ^-2)), xlab = expression(paste("FIA Litter kg/m" ^-2)))
Litterlm <-lm(FuelsWest$FIA_Litter.kg.m.2~FuelsWest$fivex_Litter.kg.m.2)
abline(Litterlm)

duff<-plot(FuelsWest$FIA_Duff.kg.m.2, FuelsWest$fivex_Duff.kg.m.2, xlim = c(0,45), ylim= c(0,23), ylab = expression(paste("LANDFIRE Duff median kg/m" ^-2)), xlab = expression(paste("FIA Duff kg/m" ^-2)))
dufflm <- lm(FuelsWest$FIA_Duff.kg.m.2~FuelsWest$fivex_Duff.kg.m.2)
abline(dufflm)

onehour<-plot(FuelsWest$FIA_1.hour.kg.m.2, FuelsWest$fivex_1.hour.kg.m.2, xlim = c(0,.5), ylim= c(0,.5), ylab = expression(paste("LANDFIRE 1 Hr Fuels median kg/m" ^-2)), xlab =expression(paste("FIA 1 Hr Fuels kg/m" ^-2)))
onehourlm <- lm(FuelsWest$FIA_1.hour.kg.m.2~FuelsWest$fivex_1.hour.kg.m.2)
abline(onehourlm)

tenhour<-plot(FuelsWest$FIA_10.hour.kg.m.2, FuelsWest$fivex_10.hour.kg.m.2, xlim = c(0,5), ylim= c(0,1), ylab = expression(paste("LANDFIRE 10 Hr Fuels median kg/m" ^-2)), xlab =expression(paste("FIA 10 Hr Fuels kg/m" ^-2)))
tenhourlm <- lm(FuelsWest$FIA_10.hour.kg.m.2~FuelsWest$fivex_10.hour.kg.m.2)
abline(tenhourlm)

hundhour<-plot(FuelsWest$FIA_100.hour.kg.m.2, FuelsWest$fivex_100.hour.kg.m.2, xlim = c(0,20), ylim= c(0,1), ylab = expression(paste("LANDFIRE 100 Hr Fuels median kg/m" ^-2)), xlab =expression(paste(" FIA 100 Hr Fuels kg/m" ^-2)))
hundhourlm <- lm(FuelsWest$FIA_100.hour.kg.m.2~FuelsWest$fivex_100.hour.kg.m.2)
abline(hundhourlm)

logs<-plot(FuelsWest$FIA_Logs.kg.m.2, FuelsWest$fivex_Logs.kg.m.2, xlim = c(0,45), ylim= c(0,11), ylab = expression(paste("LANDFIRE 1000 Hr Fuels median kg/m" ^-2)), xlab =expression(paste("FIA 1000Hr Fuels kg/m" ^-2)))
logslm <- lm(FuelsWest$FIA_Logs.kg.m.2~FuelsWest$fivex_Logs.kg.m.2)
abline(logslm)

#par(FLM_fuelcomp)

###lm
sink("results_lm.txt", append=T)
summary(Litterlm)
summary(dufflm)
summary(onehourlm)
summary(tenhourlm)
summary(hundhourlm)
summary(logslm)
sink()
## **** use 'identify(x,y)' to id individual points on the plot.  
## Run plot line first, then identify line if they don't work together...  Do one plot at at time!!!  ******

plot(FuelsWest$FLM_plot, FuelsWest$FWD_LG_DRYBIOT_AC, xlim = c(0,100), ylim= c(0,100), ylab = "100 Hr Fuels (tons per acre)", xlab =" FLMF")
identify(FuelsWest$FLM_plot, FuelsWest$FWD_LG_DRYBIOT_AC)


###
cor(FuelsWest$FIA_10.hour.kg.m.2, FuelsWest$fivex_10.hour.kg.m.2, use = "everything",
     method = c("pearson", "kendall", "spearman"))