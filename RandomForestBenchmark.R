#Read Data
#Mention the directory/location of csv file in your host/system and replace the backwards slash with double forward slash
data<-read.csv("M://Random Forest//1_random_forest_r_submission.csv",header=TRUE)
str(data)
data$Survived<-as.factor(data$Survived)
table(data$Survived)
#Data Partition
set.seed(123)
ind<-sample(2,nrow(data),replace = TRUE, prob=c(0.7,0.3))
train<-data[ind==1,]
train<-data[ind==2,]
rf<-randomForest(Survived~.,data=train)
print(rf)
# All attributes  of rf
attributes(rf)
# Prediction
rf$predicted
rf$importance
rf$call
rf$mse
# Prediction
install.packages("caret")
library(caret)
p1<-predict(rf,train)
head(p1)
head(train$Survived)
tail(train$Survived)
# Plot
plot(rf)
# Tune mtry
t <- tuneRF(train[,-22], train[,22],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)
# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "#76EE00")
# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
varUsed(rf)
# Extract Single Tree
getTree(rf, 1, labelVar = TRUE)