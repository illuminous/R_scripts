#working directory
path <- "C:/Users/jherynk/OneDrive - Washington Corporations/jh_RScripts/Trips_Worked_by_Employee"

#set working directory
setwd(path)

#Load Datasets
my_data <- read.csv("Trips_Worked_in_the_past_2_quarters.csv")
#check for missing values
table(is.na(my_data))

#Quartiles
range(my_data$Sum)
quantile(my_data$Sum, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95))


# Kernel Density Plot
d <- density(my_data$Sum) # returns the density data 
plot(d, main="Trips Worked During Q1 and Q2 2019 Kernel Density", xlab="Number of Trips") # plots the results
polygon(d, col="blue")

###box plots###
boxplot(my_data$Sum~my_data$X2019.Q2, main="Trains by Week", xlab="Weekly Summary (12/1/2012 - 5/31/2019", ylab="Trains Per Day as Measured by the Revenue Dept.")