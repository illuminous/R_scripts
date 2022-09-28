setwd("C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\R_wkspc\\")

FuelsWest<- read.table("tbl_Lutes_FIA_Combined_BoxPlots.csv",header = TRUE, sep = ",", row.names= 1)


fuelcomp <- par(mfrow=c(3,3), main = "FLM 11: FIA vs Lutes FLM Fuel Components")

###FLM 11
wisker<-boxplot(FuelsWest$X1.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="11_fia" | FuelsWest$FLM_key=="11_flm",xlim=c(4,7),xlab="1-hour: FIA vs Lutes FLM", ylab="1-hour kg m -2")
wisker<-boxplot(FuelsWest$X10.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="11_fia" | FuelsWest$FLM_key=="11_flm",xlim=c(4,7),xlab="10-hour: FIA vs Lutes FLM", ylab="10-hour kg m -2")
wisker<-boxplot(FuelsWest$X100.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="11_fia" | FuelsWest$FLM_key=="11_flm",xlim=c(4,7),xlab="100-hour: FIA vs Lutes FLM", ylab="100-hour kg m -2")
wisker<-boxplot(FuelsWest$Duff.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="11_fia" | FuelsWest$FLM_key=="11_flm",xlim=c(4,7),xlab="Duff: FIA vs Lutes FLM", ylab="Duff kg m -2")
wisker<-boxplot(FuelsWest$Litter.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="11_fia" | FuelsWest$FLM_key=="11_flm",xlim=c(4,7),xlab="Litter: FIA vs Lutes FLM", ylab="Litter kg m -2")
wisker<-boxplot(FuelsWest$FWD.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="11_fia" | FuelsWest$FLM_key=="11_flm",xlim=c(4,7),xlab="Total FWD: FIA vs Lutes FLM", ylab="FWD kg m -2")
wisker<-boxplot(FuelsWest$Total.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="11_fia" | FuelsWest$FLM_key=="11_flm",xlim=c(4,7),xlab="Total Fuel: FIA vs Lutes FLM", ylab="Total kg m -2")

###FLM 12
fuelcomp <- par(mfrow=c(3,3), main = "FLM 12: FIA vs Lutes FLM Fuel Components")
wisker<-boxplot(FuelsWest$X1.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="12_fia" | FuelsWest$FLM_key=="12_flm",xlim=c(6,9),xlab="1-hour: FIA vs Lutes FLM", ylab="1-hour kg m -2")
wisker<-boxplot(FuelsWest$X10.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="12_fia" | FuelsWest$FLM_key=="12_flm",xlim=c(6,9),xlab="10-hour: FIA vs Lutes FLM", ylab="10-hour kg m -2")
wisker<-boxplot(FuelsWest$X100.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="12_fia" | FuelsWest$FLM_key=="12_flm",xlim=c(6,9),xlab="100-hour: FIA vs Lutes FLM", ylab="100-hour kg m -2")
wisker<-boxplot(FuelsWest$Duff.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="12_fia" | FuelsWest$FLM_key=="12_flm",xlim=c(6,9),xlab="Duff: FIA vs Lutes FLM", ylab="Duff kg m -2")
wisker<-boxplot(FuelsWest$Litter.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="12_fia" | FuelsWest$FLM_key=="12_flm",xlim=c(6,9),xlab="Litter: FIA vs Lutes FLM", ylab="Litter kg m -2")
wisker<-boxplot(FuelsWest$FWD.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="12_fia" | FuelsWest$FLM_key=="12_flm",xlim=c(6,9),xlab="Total FWD: FIA vs Lutes FLM", ylab="FWD kg m -2")
wisker<-boxplot(FuelsWest$Total.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="12_fia" | FuelsWest$FLM_key=="12_flm",xlim=c(6,9),xlab="Total Fuel: FIA vs Lutes FLM", ylab="Total kg m -2")

###FLM 13
fuelcomp <- par(mfrow=c(3,3), main = "FLM 13: FIA vs Lutes FLM Fuel Components")
wisker<-boxplot(FuelsWest$X1.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="13_fia" | FuelsWest$FLM_key=="13_flm",xlim=c(8,11),xlab="1-hour: FIA vs Lutes FLM", ylab="1-hour kg m -2")
wisker<-boxplot(FuelsWest$X10.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="13_fia" | FuelsWest$FLM_key=="13_flm",xlim=c(8,11),xlab="10-hour: FIA vs Lutes FLM", ylab="10-hour kg m -2")
wisker<-boxplot(FuelsWest$X100.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="13_fia" | FuelsWest$FLM_key=="13_flm",xlim=c(8,11),xlab="100-hour: FIA vs Lutes FLM", ylab="100-hour kg m -2")
wisker<-boxplot(FuelsWest$Duff.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="13_fia" | FuelsWest$FLM_key=="13_flm",xlim=c(8,11),xlab="Duff: FIA vs Lutes FLM", ylab="Duff kg m -2")
wisker<-boxplot(FuelsWest$Litter.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="13_fia" | FuelsWest$FLM_key=="13_flm",xlim=c(8,11),xlab="Litter: FIA vs Lutes FLM", ylab="Litter kg m -2")
wisker<-boxplot(FuelsWest$FWD.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="13_fia" | FuelsWest$FLM_key=="13_flm",xlim=c(8,11),xlab="Total FWD: FIA vs Lutes FLM", ylab="FWD kg m -2")
wisker<-boxplot(FuelsWest$Total.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="13_fia" | FuelsWest$FLM_key=="13_flm",xlim=c(8,11),xlab="Total Fuel: FIA vs Lutes FLM", ylab="Total kg m -2")

###FLM 21
fuelcomp <- par(mfrow=c(3,3), main = "FLM 21: FIA vs Lutes FLM Fuel Components")
wisker<-boxplot(FuelsWest$X1.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="21_fia" | FuelsWest$FLM_key=="21_flm",xlim=c(10,13),xlab="1-hour: FIA vs Lutes FLM", ylab="1-hour kg m -2")
wisker<-boxplot(FuelsWest$X10.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="21_fia" | FuelsWest$FLM_key=="21_flm",xlim=c(10,13),xlab="10-hour: FIA vs Lutes FLM", ylab="10-hour kg m -2")
wisker<-boxplot(FuelsWest$X100.hour.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="21_fia" | FuelsWest$FLM_key=="21_flm",xlim=c(10,13),xlab="100-hour: FIA vs Lutes FLM", ylab="100-hour kg m -2")
wisker<-boxplot(FuelsWest$Duff.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="21_fia" | FuelsWest$FLM_key=="21_flm",xlim=c(10,13),xlab="Duff: FIA vs Lutes FLM", ylab="Duff kg m -2")
wisker<-boxplot(FuelsWest$Litter.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="21_fia" | FuelsWest$FLM_key=="21_flm",xlim=c(10,13),xlab="Litter: FIA vs Lutes FLM", ylab="Litter kg m -2")
wisker<-boxplot(FuelsWest$FWD.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="21_fia" | FuelsWest$FLM_key=="21_flm",xlim=c(10,13),xlab="Total FWD: FIA vs Lutes FLM", ylab="FWD kg m -2")
wisker<-boxplot(FuelsWest$Total.kg.m..2~FuelsWest$FLM_key, subset=FuelsWest$FLM_key=="21_fia" | FuelsWest$FLM_key=="21_flm",xlim=c(10,13),xlab="Total Fuel: FIA vs Lutes FLM", ylab="Total kg m -2")