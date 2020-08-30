glass <- read.csv('D:/WORK/KNN/glass.csv')
View(glass)
table(glass$Type)
# table or proportation of enteries in the datasets
round(prop.table(table(glass$Type))*100,1)
summary(glass)
#Create a function to normalize the data
nglass <- as.data.frame(apply(glass[, 1:9], 2, function(x) (x - min(x))/(max(x)-min(x))))
#attaching normal data to type
type <- (glass[,10])
glass1 <- cbind(nglass,type)
View(glass1)
#create data partitioning is other way #
library(ggplot2)
library(caret)
inTraininglocal <- createDataPartition(glass1$type,p=.70,list=F)
training<- glass1[inTraininglocal,]
View(training)
testing <- glass1[-inTraininglocal,]
View(testing)

# Build a KNN model on taining dataset
library("class")
test_acc <- NULL
train_acc <- NULL
for(i in seq(3,200,2)){
        train_glass_pred <- knn(train = training,test = training,cl=training$type,k=i)
        train_acc <- c(train_acc,mean(train_glass_pred==training$type))
        test_glass_pred <- knn(train = testing,test = testing,cl=testing$type,k=i)
        test_acc <- c(test_acc,mean(test_glass_pred==testing$type))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")
acc_glass <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
#where alaways test accuracy must be more than train accuracy
# Plotting 2 different graphs on same co-ordinate axis
library(ggplot2)
ggplot(acc_glass,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
#Below is the final method 
glass_pred <- knn(train = training,test = training,cl=training$type,k=3)
glass_pred
mean(glass_pred==training$type)
