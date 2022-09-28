setwd("C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\")

FuelsWest<- read.table("Fuel_load_west_analysis_set_20120412.csv",header = TRUE, sep = ",", row.names= 1)

sink("results_min_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$FLM_plot), min, na.rm=T)
sink()

sink("results_max_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$FLM_plot), max, na.rm=T)
sink()

sink("results_median_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$FLM_plot), median, na.rm=T)
sink()

sink("results_quantile_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$FLM_plot), quantile, na.rm=T)
sink()

sink("results_mean_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$FLM_plot), mean, na.rm=T)
sink()
####
##Boxplots
####
FLMwisker<- boxplot(FuelsWest$FIA_1.hour.kg.m.2~FuelsWest$FLM_plot, xlab = "FLM FIA", ylab= "1hour fuel")

##################
####FCCS vs WFAT
##################

setwd("C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\")

FuelsWest<- read.table("Fuel_load_west_analysis_set_20120412.csv",header = TRUE, sep = ",", row.names= 1)

sink("fccs_results_min_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$LF2008_FCCS_1x1), min, na.rm=T)
sink()

sink("fccs_results_max_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$LF2008_FCCS_1x1), max, na.rm=T)
sink()

sink("fccs_results_median_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$LF2008_FCCS_1x1), median, na.rm=T)
sink()

sink("fccs_results_quantile_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$LF2008_FCCS_1x1), quantile, na.rm=T)
sink()

sink("fccs_results_mean_5x5_Logs.txt", append=T)
aggregate(FuelsWest$FIA_Logs.kg.m.2, list(FuelsWest$LF2008_FCCS_1x1), mean, na.rm=T)
sink()


