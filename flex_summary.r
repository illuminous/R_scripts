# FTG_vs_FTGkeyed.R

## *** Updated on May 28th, 2013
## *** 

input_tbl <- 'C://working//Accounting/Flex/Payroll_xout.csv'
flex <- read.table(file=input_tbl, sep=',', header=T)

attach(flex)
newdata <- flex[order(ID, Date),]

unique(newdata$ID)
apply(newdata, 2, function(x)length(unique(x)))