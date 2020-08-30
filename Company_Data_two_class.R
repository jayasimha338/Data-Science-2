company <- read.csv("D:/WORK/DECISSION TREES/Company_Data.csv")
View(company)
table(company$Sales)
range(company$Sales)
summary(company$Sales)#having 2 classes
str(company)
mean(company$Sales)
c <- company
write.csv(c,file = "company.csv")
getwd()
str(c)
c$Sales<- ifelse(c$Sales<=5,"low","high")
table(c$Sales)
summary(c)
c$Sales <- as.factor(c$Sales)
str(c)

library(caret)
library(C50)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(c$Sales,p=.75,list=F)
training <- c[inTraininglocal,]
View(training)
prop.table(table(training$Sales))
testing <- c[-inTraininglocal,]
View(testing)
prop.table(table(testing$Sales))
#model building
model <- C5.0(training$Sales~.,data = training,trails=50)
summary(model)
plot(model)
pred <- predict.C5.0(model,testing)
table(pred)
a <- table(testing$Sales,pred)
a
sum(diag(a)/sum(a))
library(gmodels)
CrossTable(pred, testing$Sales,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


#Hence the accuracy of the model is 78.78%

