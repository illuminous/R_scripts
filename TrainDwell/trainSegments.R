trainSegments<-read.csv("TrainSegmentData.csv", header=TRUE)
str(trainSegments)

#Distribution across classes
table(trainSegments$Minutes)
table(trainSegments$StationID)

# Load up ggplot2 package to use for visulizations
library(ggplot2)

# find the mean by Code
tapply(trainSegments$Code, trainSegments$Minutes, mean)
stationmean <- aggregate(as.numeric(trainSegments$Minutes), by=list(trainSegments$Code), FUN=mean)
sortedstationmean <- stationmean[order(stationmean$Minutes)]

#box and wisker plots
wisker<-boxplot(trainSegments$Code ~ trainSegments$Minutes, data=trainSegments,xlab="Station",ylab="Minutes")
boxplot(as.numeric(Minutes)~Code, data=trainSegments, main=toupper("Dwell Time"), font.main=3, cex.main=1.2, xlab="Station", ylab="Mean (Minutes)", font.lab=3, col="blue", las=2)

# Hypothesis - MRL Bottlenecks at helena and spokane
trainSegments$Minutes <- as.factor(trainSegments$Minutes)
trainSegments$Code <- as.factor(trainSegments$Code)
ggplot(trainSegments, aes(x = Code, fill = Minutes)) + 
  stat_count(width = 0.5) + 
  xlab("Station") +
  ylab("Minutes") +
  labs(fill = "Survived")