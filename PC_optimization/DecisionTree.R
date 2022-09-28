mrlroles<-read.csv("C:/jh_RScripts/PC_optimization/MRLRolesAll_RemovedStdImage.csv", header=TRUE)
str(mrlroles)
options(max.print=1000000)


# #Non Duplicates
# y <-mrlroles[!duplicated(lapply(mrlroles, summary))]
# colnames(y)
# #Duplicates
# x <-mrlroles[duplicated(lapply(mrlroles, summary))]
# colnames(x)

#loading required libraries
 library(rpart)
 library(e1071)
 library(rpart.plot)
 library(caret)

#setting the tree control parameters
 fitControl <- trainControl(method = "cv", number = 5)
 cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree
 tree_model <- train(mrlroles ~ stuff, data = mrlroles, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
 print(tree_model)

 main_tree <- rpart(stuff ~ ., data = mrlroles, control = rpart.control(cp=0.01))
 prp(main_tree)

# library(tree)
# 
# tree.model <- tree(stuff ~ ., data=mrlroles)
# plot(tree.model)
# text(tree.model, cex=.75)