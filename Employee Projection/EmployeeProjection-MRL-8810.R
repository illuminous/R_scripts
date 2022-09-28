#load directory
 path <- "C:/Users/jherynk/OneDrive - Washington Corporations/jh_RScripts/Employee Projection"

 setwd(path)

#load data
 empCounts <- read.csv("Availability.csv")

 
 #check dimesions ( number of row & columns) in data set
 dim(empCounts)

 
 #check the variables and their types in train
 str(empCounts)
 
 #check for missing values
 table(is.na(empCounts))
 #explore missing data
 colSums(is.na(empCounts))
 
 #show more detail
 summary(empCounts)
 
 #visualize bivariate
 #installed.packages("ggplot2")
 library("ggplot2")
 ggplot(empCounts, aes(x= Available, y = Trains.per.day)) + geom_point(size = 2.5, color="navy") + xlab("Available Employees") + ylab("Trains Per Day") + ggtitle("Available Employees vs Trains Per Day")
 ##ggplot(empCounts, aes(Non.Active, Trains.per.day)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Non Active vs Trains Per Day") + theme_bw()
 lm(empCounts$Available ~ empCounts$Trains.per.day)
 #Residual values are the difference between actual and predicted outcome values. Fitted values are the predicted values. If you see carefully, you'll discover it as a funnel shape graph (from right to left ). The shape of this graph suggests that our model is suffering from heteroskedasticity (unequal variance in error terms). Had there been constant variance, there would be no pattern visible in this graph.
 
 #A common practice to tackle heteroskedasticity is by taking the log of response variable. Let's do it and check if we can get further improvement.
 
 
 linear_model <- lm(log(Trains.per.day) ~ ., data = empCounts)
 summary(linear_model)
 par(mfrow=c(2,2))
 plot(linear_model)
 
 cor(empCounts)

