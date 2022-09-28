library(dplyr)
library(tibble)
library(digest)
mrlroles<-read.csv("C:/jh_RScripts/PC_optimization/MRLRolesAll_RemovedStdImage_Reduced_Minus_Revenue.csv", header=TRUE)
#mrlroles<-read.csv("C:/jh_RScripts/PC_optimization/test.csv", header=TRUE)
str(mrlroles)
options(max.print=1000000)


#Unique Elements Non Duplicates
y <-mrlroles[!duplicated(lapply(mrlroles, digest))]
colnames(y)
#Duplicates
x <-mrlroles[duplicated(lapply(mrlroles, digest))]
colnames(x)

#Group columns
sss <- data.frame(apply(mrlroles,2,function(x) paste0(x,collapse=",")))
names(sss) <- c("pattern")
sss2 <- sss %>% rownames_to_column()
z<-lapply(unique(sss2$pattern), function(x) sss2$rowname[which(sss2$pattern==x)]) 

##write.csv(z, file = "C:/jh_RScripts/PC_optimization/MRLRolesAll_Binned2.csv")

