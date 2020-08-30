af <- read.csv(file.choose())
View(af)
str(af)
table(af$affairs)
af1 <- af
af2 <- af1
af2_con <- select(af2,3,4)
af2_cat <- select(af2,-3,-4)
View(af1)
sum(is.na(af1))
str(af1)
af1$gender <- ifelse(af1$gender=="male",1,0)
af1$children <- ifelse(af1$children=="yes",1,0)
af1$affairs <- ifelse(af1$affairs=="0",0,1)
View(af1)
table(af1$children)
#EDA
#Here x-continous and y-discrete
#Where y-affairs,x-age,yearsmarried(continous)x-gender,childern,religiousness,rating,occupation,education(categorical)
summary(af1)
boxplot(af2_con)
boxplot(af2_cat)
barplot(table(af2_cat$religiousness))
hist(af2_cat$affairs)
hist(af2_con$age)
plot(af2_cat)
scatter.smooth(af2_con)
scatter.smooth(af2_cat$affairs,af2_cat$children)
pairs(af2_cat)
pairs.panels(af2_cat)
pairs.panels(af2_con)
cor(af1)
kurtosis(af1)
skewness(af1)
# Logistic Regression 
str(af1)
logit<-glm(factor(affairs)~factor(gender)+factor(children)+age+yearsmarried+religiousness+education+occupation+rating,family=binomial,data = af1)
summary(logit)
logit1<-glm(factor(affairs)~factor(gender)+age+yearsmarried+religiousness+rating,family=binomial,data = af1)
summary(logit1)

exp(coef(logit1))
table(af1$affairs)
# Confusion matrix table 
prob <- predict(logit1,type=c("response"),af1)
prob
str(prob)
confusion<-table(prob>0.5,af1$affairs)
confusion
table(confusion)

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error
#Odd's ratio
Accuracy/Error
# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,af1$affairs)
rocrpred
rocrperf<-performance(rocrpred,'tpr','fpr')
rocrperf
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
#Since 77% of persons affairs and 23% have affairs
confusionMatrix(factor(prob, levels=1:601), factor(af1$affairs, levels=1:601))
