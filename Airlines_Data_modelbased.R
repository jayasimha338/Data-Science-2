library(readr)
library(readxl)
airlines <- read_xlsx('D:/WORK/FORECASTING/Airlines+Data.xlsx')
plot(airlines$Month,airlines$Passengers,type = 'b')
#ggplot
library(ggplot2)
ggplot(data = airlines,aes(x = airlines$Month,y = airlines$Passengers )) + 
  geom_point(color='blue') + geom_line(color='red')


a <- data.frame(outer(rep(month.abb,length=96), month.abb, '==') + 0 )
colnames(a)<-month.abb 
View(a)
a1 <- cbind(airlines,a)
a1['t'] <- c(1:96)
a1['log_pas'] <- log(a1['Passengers'])
a1['t_saq'] <- a1['t']*a1['t']
train <- a1[1:80,]
test <- a1[81:96,]

########################### LINEAR MODEL #############################
lm <- lm(Passengers~t,data = train)
summary(lm)
pred_lm <- data.frame(predict(lm,interval = 'predict',newdata = test))
rmse_lm <- sqrt(mean((test$Passengers-pred_lm$fit)^2,na.rm=T))
rmse_lm

######################### Exponential #################################
ex <- lm(log_pas~t,data=train)
summary(ex)
pred_ex <- data.frame(predict(ex,interval = 'predict',newdata = test))
rmse_ex <- sqrt(mean((test$Passengers-exp(pred_ex$fit))^2,na.rm = T))
rmse_ex

######################### Quadratic ####################################
qd <- lm(Passengers~t+t_saq,data=train)
summary(qd)
pred_qd <- data.frame(predict(qd,interval = 'predict',newdata = test))
rmse_qd <- sqrt(mean((test$Passengers-pred_qd$fit)^2,na.rm = T))
rmse_qd

######################### Additive Seasonality #########################
asm <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(asm)
pred_asm <- data.frame(predict(asm,interval = 'predict',newdata = test))
rmse_asm <- sqrt(mean((test$Passengers-pred_asm$fit)^2,na.rm=T))
rmse_asm

######################## Additive Seasonality with Quadratic #################
asqm <- lm(Passengers~t+t_saq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(asqm)
pred_asqm <- data.frame(predict(asqm,interval = 'predict',newdata = test))
rmse_asqm <- sqrt(mean((test$Passengers-pred_asqm$fit)^2,na.rm = T))
rmse_asqm

######################## Multiplicative Seasonality #########################
msm <- lm(log_pas~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(msm)
pred_msm <- data.frame(predict(msm,interval = 'predict',newdata = test))
rmse_msm <- sqrt(mean((test$Passengers-exp(pred_msm$fit))^2,na.rm = T))
rmse_msm

######################## Multiplicative Additive Seasonality ##########################
masm <- lm(log_pas~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(masm)
pred_masm <- data.frame(predict(masm,interval = 'predict',newdata = test))
rmse_masm <- sqrt(mean((test$Passengers-exp(pred_masm$fit))^2,na.rm = T))
rmse_masm

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_lm","rmse_ex","rmse_qd","rmse_asm","rmse_asqm","rmse_msm","rmse_masm"),c(rmse_lm,rmse_ex,rmse_qd,rmse_asm,rmse_asqm,rmse_msm,rmse_masm))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Additive Seasonality has least RMSE
write.csv(a1,file="a1.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
setwd('D:/R PRATICE')
test_data <- read.csv('a1.csv')
pred_new <- predict(masm,interval = 'predict',newdata = test_data)
pred_new
