library(readr)
x50 <- read.csv(file.choose())
#Converting Discrete Column into Continous Column
str(x50)
library(dummies)
x50_dummy <- dummy.data.frame(x50,sep="_")
View(x50_dummy)
# Changing column names
View(x50_dummy)
colnames(x50_dummy)[1] <- "rd"
colnames(x50_dummy)[2] <- "ad"
colnames(x50_dummy)[3] <- "ms"
colnames(x50_dummy)[4] <- "sc"
colnames(x50_dummy)[5] <- "sf"
colnames(x50_dummy)[6] <- "sn"
colnames(x50_dummy)[7] <- "pt"
attach(x50_dummy)
View(x50_dummy)
#Multi-Linear Regression
#Where all variables x1,x2,x3,x4,x5,x6 and y are continous
#y-pt,x1-rd,x2-ad,x3-ms,x4-sc,x5-sf,x6-sn
# Exploratory Data Analysis
summary(x50_dummy)
#Find the correlation b/n Output (pt) & (rd,ad,ms,sc,sf,sn)-Scatter plot
pairs(x50_dummy)
plot(x50_dummy)
library(psych)
pairs.panels(x50_dummy)
#Correlation coefficient
cor(x50_dummy)
library(corpcor)
cor2pcor(cor(x50_dummy))
#Model bulding
mlr <- lm(pt~rd+ad+ms+sc+sf+sn ,data = x50_dummy)
summary(mlr)
library(caret)
pmlr <- predict(mlr)
RMSE(x50_dummy$pt,pmlr)
RMSE()
library(VIF)
vif(mlr)
# Exponential Model
attach(x50_dummy)
mlr1_exp <- lm(log(pt) ~.,data = x50_dummy)
summary(mlr1_exp)
pexp <- predict(mlr1_exp)
RMSE(x50_dummy$pt,pexp)
#Logrithamic Model
attach(x50_dummy)
mlr2_log <- lm(pt ~ log(c(rd+ad+ms+sc+sf+sn)),data = x50_dummy)
summary(mlr2_log)
plog <- predict(mlr2_log)
summary(plog)
RMSE(x50_dummy$pt,plog)
# Polynomial model with 2 degree (quadratic model)
mlr2degree <- lm(log(pt) ~ (rd + I(rd*rd))+(ad + I(ad*ad))+(ms + I(ms*ms))+(sc + I(sc*sc))+(sf + I(sf*sf))+(sn + I(sn*sn)))
mlr2degree <- lm(log(pt) ~ c(rd+ad+ms+sc+sf+sn)+ I(c((rd*rd)+(ad*ad)+(ms*ms)+(sc*sc)+(sf*sf)+(sn*sn))))
summary(mlr2degree)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(mlr)
library(car)
## plotting Influential measures 
influenceIndexPlot(mlr,id.n=3) # index plots for infuence measures
influencePlot(mlr,id.n=3) # A user friendly representation of the above
# Regression after deleting the 50th,49th,47th observations, which is influential observation
mlr1<-lm(pt~.,data=x50_dummy[-c(50,49,47),])
summary(mlr1)
## Variance Inflation factor to check collinearity b/n variables 
library(car)
library(VIF)
vif(mlr)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(mlr2_log,id.n=2,id.cex=0.7)
qqPlot(mlr2_log,id.n = 5)
## From above we can conclude lograthmic model is significant where P-value < 0.05 ,MR^2 is moderate

