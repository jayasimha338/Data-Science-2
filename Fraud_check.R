fraud <- read.csv("D:/WORK/DECISSION TREES/Fraud_check.csv")
summary(fraud$Taxable.Income)
mean(fraud$Taxable.Income)
fraud1 <- fraud
y<- ifelse(fraud1$Taxable.Income<=30000,'risky','good')
fraud1 <- cbind(y,fraud1[,-3])
write.csv(fraud1,file = "fraud.csv")
getwd()
str(fraud1)
table(fraud1$y)
library(caret)
library(C50)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(fraud1$y,p=.75,list=F)
training <- fraud1[inTraininglocal,]
View(training)
testing <- fraud1[-inTraininglocal,]
View(testing)
#model building
model <- C5.0(training$y~.,data = training,trails=50)
summary(model)
plot(model)
pred <- predict.C5.0(model,testing)
table(pred)
a <- table(testing$y,pred)
a
sum(diag(a)/sum(a))

library(gmodels)
CrossTable(pred, testing$y,
           prop.chisq = TRUE, prop.t = F, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

#Hence the accuracy of the model is 79.33%

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(fraud1$y,p=.85,list=F)
  training1<-fraud1[inTraininglocal,]
  testing1<-fraud1[-inTraininglocal,]
  
  fittree<-C5.0(training1$y~.,data=training1)
  pred<-predict.C5.0(fittree,testing1)
  a<-table(testing1$y,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
acc
summary(acc)
mean(acc)
#Hence the accuracy of the model is 79.77%
