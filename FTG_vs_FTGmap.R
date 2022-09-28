# FTG_vs_FIAmap.R

## *** Updated on May 28, 2013
## *** Create FTG tables with only mean 
## *** 

fin <- 'E://flmfccsPaper//FLM.FCCS.FIA Research Note//Data/R_wkspc//Urbanski_Contribution//FIA_fuel_plots.csv'
FIA <- read.table(file=fin,sep=',',header=T)
## Exlcude plotid = 7725 or 12169
lbunk <- FIA[,'plotid']==7725 | FIA[,'plotid']==12169
FIA <- FIA[!lbunk,]

ftg.map <- FIA[,'FIA_Map']
### PLot FTG keyed versus FIA plot for top 12 FTG
## Top 12 map FTG
fg.12 <- sort(c(180,200,260,220,280,920,370,900,300,360,910,940))
## ton/acre to kg/m2
ta.2.kgm2 <- (1./4.46)

lll <- is.element(ftg.map,fg.12)
ftg.map <- ftg.map[lll]
fia.litter <- ta.2.kgm2*FIA[lll,"LITTER_BIOMASS"]
fia.fwd <- ta.2.kgm2*(FIA[lll,"FWD_SM_DRYBIOT_AC"]+FIA[lll,"FWD_MD_DRYBIOT_AC"]+FIA[lll,"FWD_LG_DRYBIOT_AC"])
fia.cwd <- ta.2.kgm2*FIA[lll,"CWD_DRYBIOT_AC"]
fia.duff <- ta.2.kgm2*FIA[lll,"DUFF_BIOMASS"]
fia.TotalLoad <- ta.2.kgm2*FIA[lll,"FIATotalLoad"]

## FTG data table
FTG <- read.table(file='E://flmfccsPaper//FLM.FCCS.FIA Research Note//Data/R_wkspc//Urbanski_Contribution//FTG_Table.csv',sep=',',header=T)

ftg.litter <- rep(NA,length(fia.litter))
ftg.fwd <- rep(NA,length(fia.fwd))
ftg.cwd <- rep(NA,length(fia.cwd))
ftg.duff <- rep(NA,length(fia.duff))
ftg.TotalLoad <- rep(NA,length(fia.TotalLoad))
for(k  in fg.12){
	lll <- ftg.map==k
	ftg.litter[lll] <- FTG[FTG[,'FTG']==k,'litter']	
	ftg.fwd[lll] <- FTG[FTG[,'FTG']==k,'hr1']	+ FTG[FTG[,'FTG']==k,'hr10'] +FTG[FTG[,'FTG']==k,'hr100']
	ftg.cwd[lll] <- FTG[FTG[,'FTG']==k,'cwd']	
	ftg.duff[lll] <- FTG[FTG[,'FTG']==k,'duff']	
	ftg.TotalLoad[lll] <- FTG[FTG[,'FTG']==k,'FTGTotalLoad']
}


#### Pretty plots 
fout <- 'E://flmfccsPaper//FLM.FCCS.FIA Research Note//Data/R_wkspc//Urbanski_Contribution//figs//FTG_vs_FIAmap.png'
png(file=fout,res=150,pointsize=14,width=1024,height=1024)
par(mgp=c(1.75,0.50,0),mar=c(3,4,1,0.5),mfrow=c(2,2))

# litter
xylim <- range(c(fia.litter,ftg.litter))
xxlab <- expression('FIA Plot (map) Litter (kg '*m^-2*')')
fit <- lm(ftg.litter ~ fia.litter)
yylab <- expression('FTG Litter (kg '*m^-2*')')
plot(fia.litter,ftg.litter,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.litter)),xlim=c(0,max(fia.litter)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# duff
xylim <- range(c(fia.duff,ftg.duff))
xxlab <- expression('FIA Plot (map) Duff (kg '*m^-2*')')
fit <- lm(ftg.duff ~ fia.duff)
yylab <- expression('FTG Duff (kg '*m^-2*')')
plot(fia.duff,ftg.duff,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.duff)),xlim=c(0,max(fia.duff)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# fwd
xylim <- range(c(fia.fwd,ftg.fwd))
xxlab <- expression('FIA Plot (map) FWD (kg '*m^-2*')')
fit <- lm(ftg.fwd ~ fia.fwd)
yylab <- expression('FTG FWD (kg '*m^-2*')')
plot(fia.fwd,ftg.fwd,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.fwd)),xlim=c(0,max(fia.fwd)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# cwd
xylim <- range(c(fia.cwd,ftg.cwd))
xxlab <- expression('FIA Plot (map) CWD (kg '*m^-2*')')
fit <- lm(ftg.cwd ~ fia.cwd)
yylab <- expression('FTG CWD (kg '*m^-2*')')
plot(fia.cwd,ftg.cwd,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.cwd)),xlim=c(0,max(fia.cwd)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# TotalLoad
xylim <- range(c(fia.TotalLoad,ftg.TotalLoad))
xxlab <- expression('FIA Plot (map) Total Load (kg '*m^-2*')')
fit <- lm(ftg.TotalLoad ~ fia.TotalLoad)
yylab <- expression('FTG Total Load (kg '*m^-2*')')
plot(fia.TotalLoad,ftg.TotalLoad,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.TotalLoad)),xlim=c(0,max(fia.TotalLoad)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

dev.off()

### Summary of fits
fit <- lm(ftg.TotalLoad ~ fia.TotalLoad)
summary(fit)


#### TotalLoad *******************************
> summary(fit)

Call:
lm(formula = ftg.TotalLoad ~ fia.TotalLoad)

Residuals:
     Min       1Q   Median       3Q      Max 
-15.1792  -0.7766   0.2358   0.9904   6.2406 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   4.050615   0.026028  155.62   <2e-16 ***
fia.TotalLoad 0.185959   0.003854   48.25   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 1.82 on 11188 degrees of freedom
Multiple R-squared: 0.1723,     Adjusted R-squared: 0.1722 
F-statistic:  2328 on 1 and 11188 DF,  p-value: < 2.2e-16 
# percent bias
> 100*(sum(ftg.TotalLoad - fia.TotalLoad))/sum(fia.TotalLoad)
[1] -1.472544
#RMSE
> sqrt((sum((ftg.TotalLoad - fia.TotalLoad)^2))/length(fia.TotalLoad))
[1] 4.065544

#### Litter **********************************
### Regression Fit
fit <- lm(ftg.litter ~ fia.litter)
summary(fit)
# percent bias
100*(sum(ftg.litter - fia.litter))/sum(fia.litter)
# RMSE
sqrt((sum((ftg.litter - fia.litter)^2))/length(fia.litter))

> summary(fit)

Call:
lm(formula = ftg.litter ~ fia.litter)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.3150 -0.3273 -0.1525  0.2931  1.8425 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.777451   0.006240  124.59   <2e-16 ***
fia.litter  0.203208   0.004206   48.32   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.479 on 11188 degrees of freedom
Multiple R-squared: 0.1726,     Adjusted R-squared: 0.1726 
F-statistic:  2335 on 1 and 11188 DF,  p-value: < 2.2e-16 

> # percent bias
> 100*(sum(ftg.litter - fia.litter))/sum(fia.litter)
[1] -3.517499
> # RMSE
> sqrt((sum((ftg.litter - fia.litter)^2))/length(fia.litter))
[1] 0.9832418
> 

#### Duff  **********************************
### Regression Fit
fit <- lm(ftg.duff ~ fia.duff)
summary(fit)
# percent bias
100*(sum(ftg.duff - fia.duff))/sum(fia.duff)
# RMSE
sqrt((sum((ftg.duff - fia.duff)^2))/length(fia.duff))

> fit <- lm(ftg.duff ~ fia.duff)
> summary(fit)

Call:
lm(formula = ftg.duff ~ fia.duff)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.7510 -0.2065  0.0214  0.3122  2.2302 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.579755   0.008079   195.5   <2e-16 ***
fia.duff    0.075260   0.002641    28.5   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.7004 on 11188 degrees of freedom
Multiple R-squared: 0.06769,    Adjusted R-squared: 0.06761 
F-statistic: 812.3 on 1 and 11188 DF,  p-value: < 2.2e-16 

> # percent bias
> 100*(sum(ftg.duff - fia.duff))/sum(fia.duff)
[1] -2.390268
> # RMSE
> sqrt((sum((ftg.duff - fia.duff)^2))/length(fia.duff))
[1] 2.422444
> 

#### FWD  **********************************
### Regression Fit
fit <- lm(ftg.fwd ~ fia.fwd)
summary(fit)
# percent bias
100*(sum(ftg.fwd - fia.fwd))/sum(fia.fwd)
# RMSE
sqrt((sum((ftg.fwd - fia.fwd)^2))/length(fia.fwd))

> ### Regression Fit
> fit <- lm(ftg.fwd ~ fia.fwd)
> summary(fit)

Call:
lm(formula = ftg.fwd ~ fia.fwd)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.88285 -0.13812  0.03723  0.14775  0.23677 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.473231   0.002088  226.61   <2e-16 ***
fia.fwd     0.075464   0.002729   27.66   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.1655 on 11188 degrees of freedom
Multiple R-squared: 0.06399,    Adjusted R-squared: 0.06391 
F-statistic: 764.8 on 1 and 11188 DF,  p-value: < 2.2e-16 

> # percent bias
> 100*(sum(ftg.fwd - fia.fwd))/sum(fia.fwd)
[1] 0.8880975
> # RMSE
> sqrt((sum((ftg.fwd - fia.fwd)^2))/length(fia.fwd))
[1] 0.555277
> 

#### CWD  **********************************
### Regression Fit
fit <- lm(ftg.cwd ~ fia.cwd)
summary(fit)
# percent bias
100*(sum(ftg.cwd - fia.cwd))/sum(fia.cwd)
# RMSE
sqrt((sum((ftg.cwd - fia.cwd)^2))/length(fia.cwd))










