#working directory
path <- "C:/Users/jherynk/OneDrive - Washington Corporations/jh_RScripts/RevenueTPDvsTobinTPD"

#set working directory
setwd(path)

#Load Datasets
my_data <- read.csv("revenueTPDvsTobinTPD.csv")

#Another line chart
my_data$Date <- as.Date(my_data$Date, "%m/%d/%Y")
plot(TPD.Revenue ~ TPD.Tobin, my_data, xaxt = "n", type = "l", ylab = "Trains Per Day Source Comparison")
axis(1, my_data$Date, format(my_data$Date, "%b %d %y"), cex.axis = .7)

# Add fit lines
abline(lm(my_data$TPD.Revenue ~ my_data$Date), col="red") # regression line (y~x) 
lines(lowess(my_data$Date,my_data$TPD.Revenue), col="blue") # lowess line (x,y)

#Scatterplot chart

plot(TPD.Revenue ~ TPD.Tobin, my_data, xaxt = "n", ylab = "Trains Per Day")


# Kernel Density Plot
d <- density(my_data$TPD.Revenue) # returns the density data 
ggplot(d, main="TPD Revenue Kernel Density", xlab="Count") # plots the results
polygon(d, col="red")

# Kernel Density Plot
d <- density(my_data$TPD.Tobin) # returns the density data 
plot(d, main="TPD Tobin (blue) vs TPD Revenue Kernel Density", xlab="Trains Per Day from 11/21/2018-5/31/2019") # plots the results
polygon(d, col="blue")
lines(density(my_data$TPD.Revenue))

#correlation
library("ggpubr")
ggscatter(my_data, x = "TPD.Revenue", y = "TPD.Tobin", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "TPD Revenue", ylab = "TPD Tobin")


#are the plots normal
ggqqplot(my_data$TPD.Revenue, ylab = "TPD Revene")
# wt
ggqqplot(my_data$TPD.Tobin, ylab = "TPD Tobin")

#are the distributions significantly different than normal?
shapiro.test(my_data$TPD.Revenue)
shapiro.test(my_data$TPD.Tobin)
