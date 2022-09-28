################
################
##5x5

plot(FLM5x5$FLM_Plot, FLM5x5$LF2008_FLM_5x5mode, main = "Mapped FLM vs FIA Keyed Plot FLM", xlab = "FIA FLM", ylab = "LANDFIRE FLM")
dataset = read.csv(file = "Fuel_load_west_analysis_set_20120412_2008_5x5_Mean_FLMLoading.csv")
cm <-table(dataset$FLM_plot, dataset$LF2008_FLM_5x5mode)
write.table(cm, "confusion_matrix_5x5.txt")
meca <- classAgreemefnt(cm, match.names=TRUE)

FLM11 <- dataset[(dataset$FLM_plot = 11)]


#Find the median for flm classes
dataset = read.csv(file = "Fuel_load_west_analysis_set_20120412_2008_5x5_Mean_FLMLoading.csv")
FLMfwd <- tapply(dataset$FWD_kg.sq.m, dataset$FLM_plot, median)
write.table(FLMcwd, "Median_cwd_FIA.csv", sep=",")hist

##########################
dataset = read.csv(file = "C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\Fuel_load_west_analysis_set_5x5.csv")
litterPlot <- plot(dataset$FIA_Litter.kg.m.2, dataset$fivex_Litter.kg.m.2, main = "Mapped FLM Median Litter vs FIA Plot Litter", xlab = "FIA Litter kg/m -2", ylab = "LANDFIRE FLM 5x5 Median Litter kg/m-2")
duffPlot <- plot(dataset$FIA_Duff.kg.m.2, dataset$fivex_Duff.kg.m.2, main = "Mapped FLM Median Duff vs FIA Plot Duff", xlab = "FIA Duff kg/m -2", ylab = "LANDFIRE FLM 5x5 Median Duff kg/m-2")
onehourPlot <- plot(dataset$FIA_1.hour.kg.m.2, dataset$fivex_1.hour.kg.m.2, main = "Mapped FLM Median 1-hour fuels vs FIA Plot 1-hour fuels", xlab = "FIA 1-hour kg/m -2", ylab = "LANDFIRE FLM 5x5 Median 1-hour kg/m-2")
tenhourPlot <- plot(dataset$FIA_10.hour.kg.m.2, dataset$fivex_10.hour.kg.m.2, main = "Mapped FLM Median 10-hour fuels vs FIA Plot 10-hour fuels", xlab = "FIA 10-hour kg/m -2", ylab = "LANDFIRE FLM 5x5 Median 10-hour kg/m-2")
hundredhourPlot <- plot(dataset$FIA_100.hour.kg.m.2, dataset$fivex_100.hour.kg.m.2, main = "Mapped FLM Median 100-hour fuels vs FIA Plot 100-hour fuels", xlab = "FIA 100-hour kg/m -2", ylab = "LANDFIRE FLM 5x5 Median 100-hour kg/m-2")
logsPlot <- plot(dataset$FIA_Logs.kg.m.2, dataset$fivex_Logs.kg.m.2, main = "Mapped FLM Median Logs vs FIA Plot Logs", xlab = "FIA Logs kg/m -2", ylab = "LANDFIRE FLM 5x5 Median Logs kg/m-2")

###############
###############
###1x1
plot(FLM1X1$FLM_Plot, FLM1X1$LF2008_FLM_1X1mode, main = "Mapped FLM vs FIA Keyed Plot FLM", xlab = "FIA FLM", ylab = "LANDFIRE FLM")
dataset = read.csv(file = "Fuel_load_west_analysis_set_20120412_2008_1X1_Mean_FLMLoading.csv")
cm <-table(dataset$FLM_plot, dataset$LF2008_FLM_1X1mode)
write.table(cm, "confusion_matrix_1X1.txt")
meca <- classAgreemefnt(cm, match.names=TRUE)

FLM11 <- dataset[(dataset$FLM_plot = 11)]


#Find the median for flm classes
dataset = read.csv(file = "Fuel_load_west_analysis_set_20120412_2008_1X1_Mean_FLMLoading.csv")
FLMfwd <- tapply(dataset$FWD_kg.sq.m, dataset$FLM_plot, median)
write.table(FLMcwd, "Median_cwd_FIA.csv", sep=",")hist

##########################
dataset = read.csv(file = "C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\Fuel_load_west_analysis_set_1X1.csv")
litterPlot <- plot(dataset$FIA_Litter.kg.m.2, dataset$onex_Litter.kg.m.2, main = "Mapped FLM Median Litter vs FIA Plot Litter", xlab = "FIA Litter kg/m -2", ylab = "LANDFIRE FLM 1X1 Median Litter kg/m-2")
duffPlot <- plot(dataset$FIA_Duff.kg.m.2, dataset$onex_Duff.kg.m.2, main = "Mapped FLM Median Duff vs FIA Plot Duff", xlab = "FIA Duff kg/m -2", ylab = "LANDFIRE FLM 1X1 Median Duff kg/m-2")
onehourPlot <- plot(dataset$FIA_1.hour.kg.m.2, dataset$onex_1.hour.kg.m.2, main = "Mapped FLM Median 1-hour fuels vs FIA Plot 1-hour fuels", xlab = "FIA 1-hour kg/m -2", ylab = "LANDFIRE FLM 1X1 Median 1-hour kg/m-2")
tenhourPlot <- plot(dataset$FIA_10.hour.kg.m.2, dataset$onex_10.hour.kg.m.2, main = "Mapped FLM Median 10-hour fuels vs FIA Plot 10-hour fuels", xlab = "FIA 10-hour kg/m -2", ylab = "LANDFIRE FLM 1X1 Median 10-hour kg/m-2")
hundredhourPlot <- plot(dataset$FIA_100.hour.kg.m.2, dataset$onex_100.hour.kg.m.2, main = "Mapped FLM Median 100-hour fuels vs FIA Plot 100-hour fuels", xlab = "FIA 100-hour kg/m -2", ylab = "LANDFIRE FLM 1X1 Median 100-hour kg/m-2")
logsPlot <- plot(dataset$FIA_Logs.kg.m.2, dataset$onex_Logs.kg.m.2, main = "Mapped FLM Median Logs vs FIA Plot Logs", xlab = "FIA Logs kg/m -2", ylab = "LANDFIRE FLM 1X1 Median Logs kg/m-2")
