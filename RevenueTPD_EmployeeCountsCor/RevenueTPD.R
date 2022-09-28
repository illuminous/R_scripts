#taken from https://www.analyticsvidhya.com/blog/2016/02/complete-tutorial-learn-data-science-scratch/#three
#working directory
path <- "C:/Users/jherynk/OneDrive - Washington Corporations/jh_RScripts/RevenueTPD_EmployeeCountsCor"

#set working directory
setwd(path)

#Load Datasets
my_data <- read.csv("revenueTrains2016-2019.csv")
#test <- read.csv("Test_u94Q5KV.csv")

#check dimesions ( number of row & columns) in data set
dim(my_data)
#dim(test)

#check the variables and their types in train
str(my_data)

#check for missing values
table(is.na(my_data))
#explore missing data
colSums(is.na(my_data))

#show more detail
summary(my_data)

head(my_data,6)

library("ggpubr")
ggscatter(my_data, x = "TPD.Bins", y = "Active", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Trains Per Day", ylab = "Employee Count")

hist(my_data$Available)
hist(my_data$TPD.Revenue)

#line chart API plotly
library(plotly)
plot_ly(data, x = ~my_data$TPD.Revenue, y = ~my_data$Date, type = 'scatter', mode = 'lines')

# Plot the line chart.
plot(my_data$TPD.Revenue,type = "o",col = "red", xlab = "12/01/2017-05/31/2019", ylab = "Trains Per Day", 
   main = "Trains Per Day")

#Another line chart
my_data$Date <- as.Date(my_data$Date, "%m/%d/%Y")
plot(TPD.Revenue ~ Date, my_data, xaxt = "n", type = "l", ylab = "Trains Per Day")
axis(1, my_data$Date, format(my_data$Date, "%b %d %y"), cex.axis = .7)

# Add fit lines
abline(lm(my_data$TPD.Revenue ~ my_data$Date), col="red") # regression line (y~x) 
lines(lowess(my_data$Date,my_data$TPD.Revenue), col="blue") # lowess line (x,y)

#Scatterplot chart

plot(TPD.Revenue ~ Date, my_data, xaxt = "n", ylab = "Trains Per Day")



#Quartiles
range(my_data$TPD.Revenue)
quantile(my_data$TPD.Revenue, probs = c(0.05, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95))


# Kernel Density Plot
d <- density(my_data$TPD.Revenue) # returns the density data 
plot(d, main="Trains Per Day Kernel Density") # plots the results
polygon(d, col="blue")

#Median 
median(my_data$TPD.Revenue)

#Median by Year

#Add a floor of start of weeks for each data point
library(lubridate)

#add start.of.week column variable
my_data$start.of.week <- floor_date(as.Date(my_data$Date, "%m/%d/%Y"), unit="week")

###box plots###
boxplot(my_data$TPD.Revenue~my_data$start.of.week, main="Trains by Week", xlab="Weekly Summary (12/1/2012 - 5/31/2019", ylab="Trains Per Day as Measured by the Revenue Dept.")



