tc<- read.csv(file.choose())
View(tc)
tc1 <- tc
View(tc1)
tc2 <-tc1[,-c(1,2,5,6,8,10,11,12,15,19,20,21,21:38)]
View(tc2)
#Multi-Linear Regression
#Where all variables x1,x2,x3,x4,x5,x6,x7,x8 and y are continous
#y- predicting model price,x1,x2,x3,x4,x5,x6,x7,x8 are independent variables
# Exploratory Data Analysis
summary(tc2)
#Find the correlation b/n Output (Price of model) & independent variables-Scatter plot
library(psych)
pairs(tc2)
plot(tc2)
pairs.panels(tc2)
#Correlation coefficient
cor(tc2)
library(corpcor)
cor2pcor(cor(tc2))
#Model bulding
mlr <- lm(Price~. ,data = tc2)
summary(mlr)
# Exponential Model
mlr_exp <- lm(log(Price) ~.,data = tc2)
summary(mlr_exp)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(mlr)
library(car)
## plotting Influential measures 
influenceIndexPlot(mlr,id.n=3) # index plots for infuence measures
influencePlot(mlr,id.n=3) # A user friendly representation of the above
## plotting Influential measures 
influenceIndexPlot(mlr,id.n=3) # index plots for infuence measures
influencePlot(mlr,id.n=3) # A user friendly representation of the above
# Regression after deleting the 81th,222th,961th observations, which is influential observation
mlr1<-lm(Price~.,data=tc2[-c(81,222,961),])
summary(mlr1)
vif(mlr1)
influenceIndexPlot(mlr1,id.n=3)
library(caret)
library(car)
pmlr <- predict(mlr1)
RMSE(tc2$Price,pmlr)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(mlr1,id.n=2,id.cex=0.7)
qqPlot(mlr1,id.n = 5)
## From above we can conclude the mlr1 model is significant where P-value < 0.05 ,MR^2 >0.81 
#Where RMSE value is less
# Where we have influential values in 81th,222th,961th observations are affecting the model


