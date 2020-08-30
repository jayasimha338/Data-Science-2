library(readr)
library(readxl)
cola <- read_xlsx('D:/WORK/FORECASTING/CocaCola_Sales_Rawdata.xlsx')
sum(is.na(cola))
class(cola)
#ggplot
library(ggplot2)
pred <- cola$Sales
ggplot(data = cola,aes(x =cola$Quarter,y = cola$Sales)) + 
  geom_point(color='blue') +
  geom_line(color='red')


c <- data.frame(outer(rep(month.abb,length=42), month.abb, '==') + 0 )
colnames(c)<-month.abb 
View(c)
c1 <- cbind(cola,c)
c1['t'] <- c(1:42)
c1['log_sal'] <- log(c1['Sales'])
c1['t_saq'] <- c1['t']*c1['t']
train <- c1[1:30,]
test <- c1[31:42,]

########################### LINEAR MODEL #############################
lm <- lm(Sales~t,data = train)
summary(lm)
pred_lm <- data.frame(predict(lm,interval = 'predict',newdata = test))
rmse_lm <- sqrt(mean((test$Sales-pred_lm$fit)^2,na.rm=T))
rmse_lm

######################### Exponential #################################
ex <- lm(log_sal~t,data=train)
summary(ex)
pred_ex <- data.frame(predict(ex,interval = 'predict',newdata = test))
rmse_ex <- sqrt(mean((test$Sales-exp(pred_ex$fit))^2,na.rm = T))
rmse_ex

######################### Quadratic ####################################
qd <- lm(Sales~t+t_saq,data=train)
summary(qd)
pred_qd <- data.frame(predict(qd,interval = 'predict',newdata = test))
rmse_qd <- sqrt(mean((test$Sales-pred_qd$fit)^2,na.rm = T))
rmse_qd

######################### Additive Seasonality #########################
asm <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(asm)
pred_asm <- data.frame(predict(asm,interval = 'predict',newdata = test))
rmse_asm <- sqrt(mean((test$Sales-pred_asm$fit)^2,na.rm=T))
rmse_asm

######################## Additive Seasonality with Quadratic #################
asqm <- lm(Sales~t+t_saq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(asqm)
pred_asqm <- data.frame(predict(asqm,interval = 'predict',newdata = test))
rmse_asqm <- sqrt(mean((test$Sales-pred_asqm$fit)^2,na.rm = T))
rmse_asqm

######################## Multiplicative Seasonality #########################
msm <- lm(log_sal~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(msm)
pred_msm <- data.frame(predict(msm,interval = 'predict',newdata = test))
rmse_msm <- sqrt(mean((test$Sales-exp(pred_msm$fit))^2,na.rm = T))
rmse_msm

######################## Multiplicative Additive Seasonality ##########################
masm <- lm(log_sal~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(masm)
pred_masm <- data.frame(predict(masm,interval = 'predict',newdata = test))
rmse_masm <- sqrt(mean((test$Sales-exp(pred_masm$fit))^2,na.rm = T))
rmse_masm

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_lm","rmse_ex","rmse_qd","rmse_asm","rmse_asqm","rmse_msm","rmse_masm"),c(rmse_lm,rmse_ex,rmse_qd,rmse_asm,rmse_asqm,rmse_msm,rmse_masm))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Additive Seasonality has least RMSE
write.csv(c1,file="c1.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
setwd('D:/R PRATICE')
test_data <- read.csv('c1.csv')
pred_new <- predict(masm,interval = 'predict',newdata = test_data)
pred_new
