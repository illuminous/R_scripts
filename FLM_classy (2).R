# FLM_Classy.R

## *** Updated on Dec 11, 2012
## *** 

fin <- 'C:/WorkSpace/flmfccsPaper/FLM.FCCS.FIA Research Note/Data/R_wkspc/wilcoxAnalysis/ShawnsAnalysis//fia_fuel_plots.csv'
FIA <- read.table(file=fin,sep=',',header=T)
## Exlcude plotid = 7725 or 12169
lbunk <- FIA[,'plotid']==7725 | FIA[,'plotid']==12169
FIA <- FIA[!lbunk,]

flm.keyed <- FIA[,'FLM_plot']

### PLot FLM keyed versus FIA plot for top 12 FLM
## Top 12 FLM -- 93% of plots
flm.12 <- sort(tapply(flm.keyed,flm.keyed,length),decreasing=T)
flm.12 <- as.numeric(names(flm.12[1:12]))

## ton/acre to kg/m2
ta.2.kgm2 <- (1./4.46)

lll <- is.element(flm.keyed,flm.12)
flm.keyed <- flm.keyed[lll]
fia.litter <- ta.2.kgm2*FIA[lll,"LITTER_BIOMASS"]
fia.fwd <- ta.2.kgm2*(FIA[lll,"FWD_SM_DRYBIOT_AC"]+FIA[lll,"FWD_MD_DRYBIOT_AC"]+FIA[lll,"FWD_LG_DRYBIOT_AC"])
fia.cwd <- ta.2.kgm2*FIA[lll,"CWD_DRYBIOT_AC"]
fia.duff <- ta.2.kgm2*FIA[lll,"DUFF_BIOMASS"]

nvar <- c('litter','duff','fwd','cwd')

### wilcox test, welch test
for(i in 1:4){
	fia.var <- get(paste('fia.',nvar[i],sep=''))
	willy <- matrix(NA,ncol=12,nrow=12,dimnames=list(flm.12,flm.12))
	welch <- matrix(NA,ncol=12,nrow=12,dimnames=list(flm.12,flm.12))
	for(k in 1:12){
		llk <- flm.keyed == flm.12[k]
		for(j in 1:12){
			llj <- flm.keyed == flm.12[j]
			z <- wilcox.test(fia.var[llk],fia.var[llj])
			willy[k,j] <- z$p.value
			z <- t.test(fia.var[llk],fia.var[llj])
			welch[k,j] <- z$p.value
		}
	}
	welch <- round(welch,4)
	willy <- round(willy,4)
	assign(paste('wilcox.',nvar[i],sep=''),willy)
	assign(paste('welch.',nvar[i],sep=''),welch)
}



sum(wilcox.litter > 0.05)-12
1 -(sum(wilcox.litter > 0.05)-12)/132

sum(wilcox.duff > 0.05)-12
1 - (sum(wilcox.duff > 0.05)-12)/132

sum(wilcox.fwd > 0.05)-12
1 -(sum(wilcox.fwd > 0.05)-12)/132

sum(wilcox.cwd > 0.05)-12
1 - (sum(wilcox.cwd > 0.05)-12)/132

> sum(wilcox.litter > 0.05)-12
[1] 28
> 1 -(sum(wilcox.litter > 0.05)-12)/132
[1] 0.7878788
> 
> sum(wilcox.duff > 0.05)-12
[1] NA
> 1 - (sum(wilcox.duff > 0.05)-12)/132
[1] NA
> 
> sum(wilcox.fwd > 0.05)-12
[1] 14
> 1 -(sum(wilcox.fwd > 0.05)-12)/132
[1] 0.8939394
> 
> sum(wilcox.cwd > 0.05)-12
[1] 8
> 1 - (sum(wilcox.cwd > 0.05)-12)/132
[1] 0.9393939




sum(welch.litter > 0.05)-12
1 -(sum(welch.litter > 0.05)-12)/132

sum(welch.duff > 0.05)-12
1 - (sum(welch.duff > 0.05)-12)/132

sum(welch.fwd > 0.05)-12
1 -(sum(welch.fwd > 0.05)-12)/132

sum(welch.cwd > 0.05)-12
1 - (sum(welch.cwd > 0.05)-12)/132


> sum(welch.litter > 0.05)-12
[1] 66
> 1 -(sum(welch.litter > 0.05)-12)/132
[1] 0.5
> 
> sum(welch.duff > 0.05)-12
[1] 52
> 1 - (sum(welch.duff > 0.05)-12)/132
[1] 0.6060606
> 
> sum(welch.fwd > 0.05)-12
[1] 32
> 1 -(sum(welch.fwd > 0.05)-12)/132
[1] 0.7575758
> 
> sum(welch.cwd > 0.05)-12
[1] 8
> 1 - (sum(welch.cwd > 0.05)-12)/132
[1] 0.9393939
> 



