###########
####Lutes FLM Table
###########
setwd("C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\lutesOut\\")

Lutestbl<- read.table("Lutes_FLM_English_Metric.csv",header = TRUE, sep = ",", row.names= 1)

sink("lutes_results_min_CWDBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$CWDBiokg.sq_m, list(Lutestbl$FINAL.FLM), min, na.rm=T)
sink()

sink("lutes_results_max_CWDBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$CWDBiokg.sq_m, list(Lutestbl$FINAL.FLM), max, na.rm=T)
sink()

sink("lutes_results_median_CWDBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$CWDBiokg.sq_m, list(Lutestbl$FINAL.FLM), median, na.rm=T)
sink()

sink("lutes_results_sd_CWDBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$CWDBiokg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()

sink("lutes_results_quantile_CWDBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$CWDBiokg.sq_m, list(Lutestbl$FINAL.FLM), quantile, na.rm=T)
sink()

sink("lutes_results_mean_CWDBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$CWDBiokg.sq_m, list(Lutestbl$FINAL.FLM), mean, na.rm=T)
sink()


###
#Box and Wisker Plots FIA vs Lutes
###
setwd("C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\")

Lutestbl<- read.table("BoxWisker.csv",header = TRUE, sep = ",", row.names= 1)
wiskers<-boxplot(Lutestbl$X1hrkg.sq_m~Lutestbl$FINAL.FLM, xlab ="FLM", ylaab = "1hr kg sq meters")

######
##Standard Deviation additions

sink("lutes_results_sd_CWDBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$CWDBiokg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()

sink("lutes_results_sd_1hr_kg.sq.m.txt", append=T)
aggregate(Lutestbl$X1hrkg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()

sink("lutes_results_sd_10hr_kg.sq.m.txt", append=T)
aggregate(Lutestbl$X10hrkg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()

sink("lutes_results_sd_100hr_kg.sq.m.txt", append=T)
aggregate(Lutestbl$X100hrkg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()

sink("lutes_results_sd_DuffBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$DuffBiokg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()

sink("lutes_results_sd_LittBio_kg.sq.m.txt", append=T)
aggregate(Lutestbl$LittBiokg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()

sink("lutes_results_sd_TotFWD_kg.sq.m.txt", append=T)
aggregate(Lutestbl$TotFWDkg.sq_m, list(Lutestbl$FINAL.FLM), sd, na.rm=T)
sink()