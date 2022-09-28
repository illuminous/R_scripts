setwd("C:\\WorkSpace\\flmfccsPaper\\FLM.FCCS.FIA Research Note\\Data\\")

FuelsWest<- read.table("FuelsWest.csv",header = TRUE, sep = ",", row.names= 1)

########################### 2-Way Frequency Table for keyed FLM to LF 1x1 matrix
attach(FuelsWest)
FLMtable <- table(FuelsWest$FLM_plot,FuelsWest$LF2008_FLM_1x1) # first (A) will be rows, second (B) will be columns 
FLMtable # print table 

margin.table(FLMtable, 1) # A frequencies (summed over columns) 
margin.table(FLMtable, 2) # B frequencies (summed over rows)

prop.table(FLMtable) # cell percentages
prop.table(FLMtable, 1) # row percentages 
prop.table(FLMtable, 2) # column percentages 


### Tests for independence
chisq.test(FLMtable) 

kappa(FLMtable)

########################### 2-Way Frequency Table for keyed FLM to LF 5x5 matrix

attach(FuelsWest)
FLM_5x5_table <- table(FuelsWest$FLM_plot,FuelsWest$LF2008_FLM_5x5) # A will be rows, B will be columns 
FLM_5x5_table # print table 

margin.table(FLM_5x5_table, 1) # A frequencies (summed over columns) 
margin.table(FLM_5x5_table, 2) # B frequencies (summed over rows)

prop.table(FLM_5x5_table) # cell percentages
prop.table(FLM_5x5_table, 1) # row percentages 
prop.table(FLM_5x5_table, 2) # column percentages 

### Tests for independence
chisq.test(FLM_5x5_table) 

kappa(FLM_5x5_table)

