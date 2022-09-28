## FTG_Figure.R

## *** Updated on NOv 28, 2012
## *** Create FTG tables with only mean 
## *** 

FIA <- read.table(file='work//fia_fuel_plots.csv',sep=',',header=T)

## Exlcude plotid = 7725 or 12169
lbunk <- FIA[,'plotid']==7725 | FIA[,'plotid']==12169
FIA <- FIA[!lbunk,]

# No. plots per FIA forgrp 
plts <- tapply(FIA[,'FORGRP'],FIA[,'FORGRP'],length)

# Read in RSAC/FIA FTG map data for western US 
# count is count of 250m x 250 m pixels
fin <- 'work//FORGRP_Map_westUS.csv'
hdr <- scan(file=fin,sep=',',nlines=1,what='char')
ftg.map <- matrix(scan(file=fin,sep=',',skip=1),ncol=length(hdr),byrow=T,dimnames=list(NULL,hdr))


## Use the top 12 categories which account for 98.6% of forested area in the western U.S.

# see G:\Projects\WFEI2\scripts\BurnGrids.R
#> round(100*yf,1)
# 180  370  200  220  260  920  280  940  360  950  900  340  300 
#20.4 18.1 15.7 14.1 12.7 10.2  4.8  1.5  1.1  0.6  0.4  0.3  0.1 
#fg.ba <- c(180,370,200,220,260,920,280,940,360,950,900,340,300)

## Updated based on 2003 - 2010 MTBS burned area (mask 1)
## See: RoughEI/check.R
##> round(100*cumsum(fa),1)
##  200   180   220   260   370   920   280   360   940   900   950   320   340 
##  19.3  37.1  53.2  69.1  79.7  89.5  96.6  97.5  98.3  99.0  99.6  99.8  99.9 
## Top 11 cover types

#fg.ba <- c(200,180,220,260,370,920,280,360,940,900,950)
#fg.11 <- fg.ba[c(200,180,220,260,370,920,280,360,940,900,950)]
#fg.12 <- ftg.map[,1]


plts[as.character(fg.12)]

## > plts[as.character(fg.12)]
# 180  200  260  220  280  920  370  900  300  360  910  940 
# 1875 2498 1813 1327  860 1084  782  302  397  598  189  229 

### go though'emall
fg.all <- sort(unique(FIA[,'FORGRP']))
plts[as.character(fg.all)]


nvar <- c('litter','hr1','hr10','hr100','cwd','duff','fwd','dbd','dz','dz_litt')
cn <- c('plotid','forgrp',nvar)
fia <- matrix(NA,ncol=length(cn),nrow=nrow(FIA),dimnames=list(NULL,cn))
fwd <- apply(FIA[,c( "FWD_SM_DRYBIOT_AC","FWD_MD_DRYBIOT_AC","FWD_LG_DRYBIOT_AC")],1,sum)
dz <- FIA[,"DUFF_DEPTH"]
fia[,'hr1'] <- FIA[, "FWD_SM_DRYBIOT_AC"]
fia[,'hr10'] <- FIA[, "FWD_MD_DRYBIOT_AC"]
fia[,'hr100'] <- FIA[, "FWD_LG_DRYBIOT_AC"]
fia[,'fwd'] <- fwd
fia[,'dz'] <- FIA[,"DUFF_DEPTH"]
fia[,'cwd'] <- FIA[,"CWD_DRYBIOT_AC"]
fia[,'duff'] <- FIA[,"DUFF_BIOMASS"]
fia[,'litter'] <- FIA[,"LITTER_BIOMASS"]
fia[,'dz_litt'] <- FIA[,"LITTER_DEPTH"]
fia[fia[,'duff'] < 0.01,'dz'] <- 0
fia[,'dbd'] <- fia[,'duff']/fia[,'dz']
lbunk <- is.na(fia[,'dbd']) | !is.finite(fia[,'dbd'])
fia[lbunk,'dbd'] <- 0
fia[,'plotid'] <- FIA[,'plotid']
fia[,'forgrp'] <- FIA[,"FORGRP"]

###
library(Hmisc)


#### Also, try a four-panel png
## plot only top 12
fg.ba <- c(180,200,220,260,280,300,360,370,900,910,920,940)
NVAR <- nvar[c(1,6,7,5)]
snvar <- c('Litter','Duff','FWD','CWD')
xxlab <- substr(as.character(fg.ba),1,3)

fout <- 'FigS1.png'
png(file=fout,res=150,pointsize=12,width=1024,height=1024)
par(mgp=c(1.75,0.50,0),mar=c(3,4,1,0.5),mfrow=c(2,2))
for(k in 1:4){
	var <- NVAR[k]
	y25 <- rep(NA,length(fg.ba))
	y75 <- rep(NA,length(fg.ba))
	y05 <- rep(NA,length(fg.ba))
	y95 <- rep(NA,length(fg.ba))
	ymed <- rep(NA,length(fg.ba))
	ymu <- rep(NA,length(fg.ba))
	xpoly <- NULL
	ypoly <- NULL
	for(i in 1:length(fg.ba)){
		fg1 <- fg.ba[i]
		lfg <- fia[,'forgrp']==fg1
		y <- fia[lfg,var]
		## Convert from ton/acre to kg/m2
		y <- y/4.46 
		yp <- quantile(y,probs=c(.05,.25,0.75,.95))
		y25[i] <- yp['25%']
		y75[i] <- yp['75%']
		y05[i] <- yp['5%']
		y95[i] <- yp['95%']
		ymed[i] <- median(y)
		ymu[i] <- mean(y)
		xpoly <- cbind(xpoly,c(i-.25,i-.25,i+.25,i+.25))
		ypoly <- cbind(ypoly,c(y25[i],y75[i],y75[i],y25[i]))
	}
	if(k==1){ylb <- expression('Litter Loading (kg '~m^-2~')')}
	if(k==2){ylb <- expression('FWD Loading (kg '~m^-2~')')}
	if(k==3){ylb <- expression('CWD Loading (kg '~m^-2~')')}
	if(k==4){ylb <- expression('Duff Loading (kg '~m^-2~')')}
	mlb <- paste('FIA plots - ',var,' loading',sep='') 
	x <- 1:length(fg.ba)
	yylim <- range(y05,y95)
	xxlim <- c(.90,12.1)
	#x11()
	plot(x,ymu,ylim=yylim,axes=F,xlab='FTG Model',ylab=ylb,xlim=xxlim)
	for(i in 1:length(fg.ba)){polygon(x=xpoly[,i],y=ypoly[,i],lwd=1.5)}
	for(i in 1:length(fg.ba)){lines(x=xpoly[c(1,4),i],y=rep(ymed[i],2),lwd=3)}
	errbar(x,ymu,yplus=y95,yminus=y05,cap=0.05,lwd=1.5,add=T)
	axis(1,at=x,labels=xxlab,lwd=1.5)
	axis(2,lwd=1.5)
	box(lwd=1.5)
	#mtext(side=3,line=0.1,mlb)
} # next component
dev.off()





