bank <- read.table(file.choose(),header = T,sep = ";")
View(bank)
str(bank)
sum(is.na(bank))
bd <- bank
bd1 <- bd

bd1_con <- select(bd1,1,6,10,12,13,14,15)
bd1_cat <- select(bd1,-1,-6,-10,-12,-13,-14,-15)
bd$default<- ifelse(bd$default=="yes",1,0)
bd$housing<- ifelse(bd$housing=="yes",1,0)
bd$loan<- ifelse(bd$loan=="yes",1,0)
bd$y<- ifelse(bd$y=="yes",1,0)
bd$poutcome = factor(bd$poutcome, levels = c('unknown','failure','other','success'), labels = c(1:4))
bd$contact = factor(bd$contact, levels = c('cellular','unknown','telephone'), labels = c(1:3))
bd$marital = factor(bd$marital, levels = c('married','single','divorced'), labels = c(1:3))
bd$month = factor(bd$month, levels = c("jan", "feb" ,"mar", "apr", "may" ,"jun" ,"jul", "aug", "sep", "oct" ,"nov", "dec"), labels = c(1:12))
bd$education = factor(bd$education, levels = c('primary','secondary','tertiary','unknown'), labels = c(1:4))
bd$job = factor(bd$job, levels = c("blue-collar","management","technician","admin.","services","retired","self-employed","entrepreneur","unemployed","housemaid","student","unknown"), labels = c(1:12))
View(bd)
str(bd)
sum(is.na(bd))
write.csv(bd,file="bd.csv",col.names = F,row.names = F)

read.csv('bd.csv')
#EDA
#Here x-continous and y-discrete
#Where y-deposit or not,x-age,balance,day,duration,pdays,previous,campaign(continous)x-job,education,month,marital,contact,poutcome,loan,housing,default(categorical)
summary(bd1)
boxplot(bd1_cat)
boxplot(bd1_con)
barplot(table(bd1_cat$job))
plot(bd1)
scatter.smooth(bd1_cat)
hist(bd1_con$balance)
cor(bd1_con)
pairs.panels(bd1_con)
# Logistic Regression 
str(bd)
logit<-glm(y~factor(job)+factor(marital)+factor(education)+factor(contact)+factor(month)+factor(poutcome)+age+default+balance+loan+day+duration+campaign+previous+pdays,family=binomial,data = bd)
summary(logit)
logit1<-glm(y~factor(marital)+factor(education)+factor(contact)+factor(month)+age+balance+loan+day+duration+campaign,family=binomial,data = bd)
summary(logit1)
exp(coef(logit1))
table(bd$y)
# Confusion matrix table 
prob <- predict(logit1,type=c("response"),bd)
prob
confusion<-table(prob>0.5,bd$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error
#Odd's ratio
Accuracy/Error
# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,bd$y)
rocrpred
rocrperf<-performance(rocrpred,'tpr','fpr')
rocrperf
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
#Since  89.5% of persons didn't deposits and 10.4% have deposits
#The client has didn't subscribed a term deposit 