# FTG_vs_FTGkeyed.R

## *** Updated on May 28th, 2013
## *** 

fin <- 'E://flmfccsPaper//FLM.FCCS.FIA Research Note//Data/R_wkspc//Urbanski_Contribution//FIA_fuel_plots.csv'
FIA <- read.table(file=fin,sep=',',header=T)
## Exlcude plotid = 7725 or 12169
lbunk <- FIA[,'plotid']==7725 | FIA[,'plotid']==12169
FIA <- FIA[!lbunk,]

ftg.keyed <- FIA[,'FORGRP']
### PLot FTG keyed versus FIA plot for top 12 FTG
## Top 12 map FTG
fg.12 <- sort(c(180,200,260,220,280,920,370,900,300,360,910,940))
## ton/acre to kg/m2
ta.2.kgm2 <- (1./4.46)

lll <- is.element(ftg.keyed,fg.12)
ftg.keyed <- ftg.keyed[lll]
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
	lll <- ftg.keyed==k
	ftg.litter[lll] <- FTG[FTG[,'FTG']==k,'litter']	
	ftg.fwd[lll] <- FTG[FTG[,'FTG']==k,'hr1']	+ FTG[FTG[,'FTG']==k,'hr10'] +FTG[FTG[,'FTG']==k,'hr100']
	ftg.cwd[lll] <- FTG[FTG[,'FTG']==k,'cwd']	
	ftg.duff[lll] <- FTG[FTG[,'FTG']==k,'duff']
	ftg.TotalLoad[lll] <- FTG[FTG[,'FTG']==k,'FTGTotalLoad']
}


#### Pretty plots 
fout <- 'E://flmfccsPaper//FLM.FCCS.FIA Research Note//Data/R_wkspc//Urbanski_Contribution//figs//FTG_vs_FIAkeyed.png'
png(file=fout,res=150,pointsize=14,width=1024,height=1024)
par(mgp=c(1.75,0.50,0),mar=c(3,4,1,0.5),mfrow=c(3,3))

# litter
xylim <- range(c(fia.litter,ftg.litter))
xxlab <- expression('FIA Plot (keyed) Litter (kg '*m^-2*')')
fit <- lm(ftg.litter ~ fia.litter)
yylab <- expression('FTG Litter (kg '*m^-2*')')
plot(fia.litter,ftg.litter,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.litter)),xlim=c(0,max(fia.litter)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# duff
xylim <- range(c(fia.duff,ftg.duff))
xxlab <- expression('FIA Plot (keyed) Duff (kg '*m^-2*')')
fit <- lm(ftg.duff ~ fia.duff)
yylab <- expression('FTG Duff (kg '*m^-2*')')
plot(fia.duff,ftg.duff,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.duff)),xlim=c(0,max(fia.duff)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# fwd
xylim <- range(c(fia.fwd,ftg.fwd))
xxlab <- expression('FIA Plot (keyed) FWD (kg '*m^-2*')')
fit <- lm(ftg.fwd ~ fia.fwd)
yylab <- expression('FTG FWD (kg '*m^-2*')')
plot(fia.fwd,ftg.fwd,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.fwd)),xlim=c(0,max(fia.fwd)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# cwd
xylim <- range(c(fia.cwd,ftg.cwd))
xxlab <- expression('FIA Plot (keyed) CWD (kg '*m^-2*')')
fit <- lm(ftg.cwd ~ fia.cwd)
yylab <- expression('FTG CWD (kg '*m^-2*')')
plot(fia.cwd,ftg.cwd,xlab=xxlab,ylab=yylab,axes=F,ylim=c(0,max(ftg.cwd)),xlim=c(0,max(fia.cwd)))
axis(1,lwd=1.5)
axis(2,lwd=1.5)
box(lwd=1.5)
abline(fit$coef)

# TotalLoad
xylim <- range(c(fia.TotalLoad,ftg.TotalLoad))
xxlab <- expression('FIA Plot (keyed) Total Load (kg '*m^-2*')')
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





#######################

> summary(fit)

Call:
lm(formula = ftg.TotalLoad ~ fia.TotalLoad)

Residuals:
     Min       1Q   Median       3Q      Max 
-18.9463  -1.3222   0.1753   1.1047   6.4199 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   3.698724   0.025351  145.90   <2e-16 ***
fia.TotalLoad 0.232413   0.003863   60.16   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 1.884 on 11952 degrees of freedom
Multiple R-squared: 0.2325,     Adjusted R-squared: 0.2324 
F-statistic:  3620 on 1 and 11952 DF,  p-value: < 2.2e-16 
# percent bias
> 100*(sum(ftg.TotalLoad - fia.TotalLoad))/sum(fia.TotalLoad)
[1] 0.07282289
#RMSE
> sqrt((sum((ftg.TotalLoad - fia.TotalLoad)^2))/length(fia.TotalLoad))
[1] 3.907582


#################################
### Summary of fits
fit <- lm(ftg.litter ~ fia.litter)
summary(fit)
> summary(fit)

Call:
lm(formula = ftg.litter ~ fia.litter)

Residuals:
    Min      1Q  Median      3Q     Max 
-5.9752 -0.3014 -0.1475  0.2447  1.8825 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.737528   0.005942  124.12   <2e-16 ***
fia.litter  0.266166   0.004040   65.88   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.4747 on 11952 degrees of freedom
Multiple R-squared: 0.2664,     Adjusted R-squared: 0.2664 
F-statistic:  4341 on 1 and 11952 DF,  p-value: < 2.2e-16 


> 100*sum(ftg.litter- fia.litter)/sum(fia.litter)
[1] 0.06429321

sqrt((sum((ftg.litter- fia.litter)^2))/length(fia.litter))

> sqrt((sum((ftg.litter- fia.litter)^2))/length(fia.litter))
[1] 0.9204691


fit <- lm(ftg.duff ~ fia.duff)
summary(fit)

> summary(fit)

Call:
lm(formula = ftg.duff ~ fia.duff)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.0633 -0.3255  0.0104  0.3465  2.3027 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.507312   0.008168  184.54   <2e-16 ***
fia.duff    0.096742   0.002705   35.76   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.7443 on 11952 degrees of freedom
Multiple R-squared: 0.09666,    Adjusted R-squared: 0.09658 
F-statistic:  1279 on 1 and 11952 DF,  p-value: < 2.2e-16 


100*(sum(ftg.duff - fia.duff)/sum(fia.duff))
sqrt((sum((ftg.duff- fia.duff)^2))/length(fia.duff))



### FWD ***************************************************************

fit <- lm(ftg.fwd ~ fia.fwd)
summary(fit)

> summary(fit)

Call:
lm(formula = ftg.fwd ~ fia.fwd)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.17182 -0.13446  0.02955  0.14326  0.27129 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.438712   0.002051  213.92   <2e-16 ***
fia.fwd     0.097269   0.002733   35.59   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.1712 on 11952 degrees of freedom
Multiple R-squared: 0.09582,    Adjusted R-squared: 0.09574 
F-statistic:  1267 on 1 and 11952 DF,  p-value: < 2.2e-16 


100*(sum(ftg.fwd - fia.fwd)/sum(fia.fwd))
sqrt((sum((ftg.fwd- fia.fwd)^2))/length(fia.fwd))


### CWD ***************************************************************


fit <- lm(ftg.cwd ~ fia.cwd)
summary(fit)


> summary(fit)

Call:
lm(formula = ftg.cwd ~ fia.cwd)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.3596 -0.8029 -0.1587  0.6958  4.6590 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.245847   0.011488  108.45   <2e-16 ***
fia.cwd     0.248889   0.003953   62.96   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 1.032 on 11952 degrees of freedom
Multiple R-squared: 0.2491,     Adjusted R-squared: 0.249 
F-statistic:  3964 on 1 and 11952 DF,  p-value: < 2.2e-16 


100*(sum(ftg.cwd - fia.cwd)/sum(fia.cwd))
sqrt((sum((ftg.cwd- fia.cwd)^2))/length(fia.cwd))

> 100*(sum(ftg.cwd - fia.cwd)/sum(fia.cwd))
[1] 0.08701851
> sqrt((sum((ftg.cwd- fia.cwd)^2))/length(fia.cwd))
[1] 2.069066



