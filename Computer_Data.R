cd<- read.csv(file.choose())
View(cd)
cmd <- cd
cmd$cd <- ifelse(cmd$cd=="yes",1,0)
cmd$multi <- ifelse(cmd$multi=="yes",1,0)
cmd$premium <- ifelse(cmd$premium=="yes",1,0)
View(cmd)
cmd1 <-cmd[,-1]
mean(cmd1$price)
sum(is.na(cmd1))
View(cmd1)
#Multi-Linear Regression
#Where all variables x1,x2,x3,x4,x5,x6,x7,x8,x9 and y are continous
#y-price of computer,x1,x2,x3,x4,x5,x6,x7,x8,x9 are independent variables
# Exploratory Data Analysis
summary(cmd1)
#Find the correlation b/n Output (price of computer ) & independent variables-Scatter plot
library(psych)
pairs.panels(cmd1)
boxplot(cmd1)
hist(cmd1$price)
plot(cmd1)
pairs.panels(cmd1)
#Correlation coefficient
cor(cmd1)
library(corpcor)
cor2pcor(cor(cmd1))
#Model bulding
mlr <- lm(price~. ,data = cmd1)
summary(mlr)
library(VIF)
vif(mlr)
library(caret)
library(car)
pmlr <- predict(mlr)
mean(pmlr)
RMSE(cmd1$price,pmlr)
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(mlr,id.n=2,id.cex=0.7)
qqPlot(mlr,id.n = 5)
## From above we can conclude the model is significant where P-value < 0.05 ,MR^2 is moderate
#Where RMSE value is less


