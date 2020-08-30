library(readr)
plastic <- read.csv('D:/WORK_RSTUDIO/FORECASTING/PlasticSales.csv')
View(plastic)
plot(plastic$Month,plastic$Sales, type = 'b')
#ggplot
library(ggplot2)
ggplot(data = plastic,aes(x =plastic$Month,y =plastic$Sales )) + 
  geom_point(color='blue') +
  geom_line(color='red')

p <- data.frame(outer(rep(month.abb,length=60), month.abb, '==') + 0 )
colnames(p)<-month.abb 
View(p)
p1 <- cbind(plastic,p)
p1['t'] <- c(1:60)
p1['log_sal'] <- log(p1['Sales'])
p1['t_saq'] <- p1['t']*p1['t']
train <- p1[1:45,]
test <- p1[46:60,]

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
write.csv(p1,file="p1.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
setwd('D:/R PRATICE')
test_data <- read.csv('p1.csv')
pred_new <- predict(masm,interval = 'predict',newdata = test_data)
pred_new
