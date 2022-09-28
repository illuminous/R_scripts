#taken from https://www.analyticsvidhya.com/blog/2016/02/complete-tutorial-learn-data-science-scratch/#three
#working directory
path <- "C:/jh_RScripts/trainingData"

#set working directory
setwd(path)

#Load Datasets
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

#check dimesions ( number of row & columns) in data set
dim(train)
dim(test)

#check the variables and their types in train
str(train)

#check for missing values
table(is.na(train))
#explore missing data
colSums(is.na(train))

#show more detail
summary(train)

#visualize bivariate
#installed.packages("ggplot2")
#library("ggplot2")
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")

#more visulization - which outlets thrive?
ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()

#which contributed to the highest sales?
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

#box and whisker sales - black point is an outlier
ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

#combine data since the two datasets have different column lengths.
test$Item_Outlet_Sales <-  1
combi <- rbind(train, test)

#impute missing values using the median 
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))

#impute missing value for Item_Visibility
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                median(combi$Item_Visibility), combi$Item_Visibility)

#correct mismatched levels in categorical variables
levels(combi$Outlet_Size)[1] <- "Other"
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)

#count outlet store identifiers - chances are the more id's the more the sales contributed by it.
#install magrittr package to use "%>%"

install.packages("magrittr")
library(magrittr)

library(dplyr)
a <- combi%>%
  group_by(Outlet_Identifier)%>%
  tally()

head(a)

names(a)[2] <- "Outlet_Count"
combi <- full_join(a, combi, by = "Outlet_Identifier")

#Count item identifiers
b <- combi%>%
  group_by(Item_Identifier)%>%
  tally()

names(b)[2] <- "Item_Count"
head (b)
combi <- merge(b, combi, by = "Item_Identifier")

#exam outlet store years - theory is the older the more sales
c <- combi%>%
  select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)
head(c)
combi <- full_join(c, combi)

#new item types to capture "FD","DR","NC". 
# q <- substr(combi$Item_Identifier,1,2)
# q <- gsub("FD","Food",q)
# q <- gsub("DR","Drinks",q)
# q <- gsub("NC","Non-Consumable",q)
# table(q)


#Label Encoding and One Hot Encoding
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1,sample))
head(demo_sample)

library(dummies)
combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'),  sep='_')
str (combi)

#Drop Variables
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content,                                Outlet_Establishment_Year,Item_Type))
str(combi)

#Begin Predictive Modeling
#Divide the datasets
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

#Build linear model for regression
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)

#correlation is bad amoung variables - find out which ones are correlated
cor(new_train)
cor(new_train$Outlet_Count, new_train$`Outlet_Type_Grocery Store`)