library(readr)
library(forecast)
library(fpp)
library(smooth)
library(tseries)
plastic <- read.csv('D:/WORK_RSTUDIO/FORECASTING/PlasticSales.csv')
View(plastic)
plot(plastic$Month,plastic$Sales, type = 'b')
#ggplot
library(ggplot2)
ggplot(data = plastic,aes(x =plastic$Month,y =plastic$Sales )) + 
  geom_point(color='blue') +
  geom_line(color='red')
class(plastic)
p <- ts(plastic$Sales,frequency = 12,start = c(49))
View(p)
train <- p[1:48]
test <- p[49:60]
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)
plot(p)
#### 1.USING HoltWinters function ################
# Optimum values
# with alpha = 0.2
hwa <- HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hwa
pred_hwa <- forecast(hwa)
pred_hwa <- data.frame(predict(hwa,n.ahead = 12))
plot(forecast(hwa,h=12))
mape_hwa <- MAPE(pred_hwa$fit,test)*100
mape_hwa
# with alpha = 0.2, beta = 0.1
hwab <- HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hwab
pred_hwab <- forecast(hwab)
pred_hwab <- data.frame(predict(hwab,n.ahead = 12))
plot(forecast(hwab,h=12))
mape_hwab <- MAPE(pred_hwab$fit,test)*100
mape_hwab
# with alpha = 0.2, beta = 0.1, gamma = 0.1 
hwabg <- HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hwabg
pred_hwabg <- forecast(hwabg)
pred_hwabg <- data.frame(predict(hwabg,n.ahead = 12))
plot(forecast(hwabg,h=12))
mape_hwabg <- MAPE(pred_hwabg$fit,test)*100
mape_hwabg
# Without optimum values aplha,beta,gamma #
# no alpha
hwna <- HoltWinters(train,beta = F,gamma = F)
hwna
pred_hwna <- forecast(hwna)
pred_hwna <- data.frame(predict(hwna,n.ahead = 12))
plot(forecast(hwna,h=12))
mape_hwna <- MAPE(pred_hwna$fit,test)*100
mape_hwna
# no alpha,no beta
hwnab <- HoltWinters(train,gamma = F)
hwnab
pred_hwnab <- forecast(hwnab)
pred_hwnab <- data.frame(predict(hwnab,n.ahead = 12))
plot(forecast(hwnab,h=12))
mape_hwnab <- MAPE(pred_hwnab$fit,test)*100
mape_hwnab
# no alpha,no beta,no gamma
hwnabg <- HoltWinters(train)
hwnabg
pred_hwnabg <- forecast(hwnabg)
pred_hwnabg <- data.frame(predict(hwnabg,n.ahead = 12))
plot(forecast(hwnabg,h=12))
mape_hwnabg <- MAPE(pred_hwnabg$fit,test)*100
mape_hwnabg
#creating table for MAPE
table_mape <- data.frame(c('mape_hwa','mape_hwab','mape_hwabg','mape_hwna','mape_hwnab','mape_hwnabg'),c(mape_hwa,mape_hwab,mape_hwabg,mape_hwna,mape_hwnab,mape_hwnabg))
colnames(table_mape)<-c("MAPE","VALUES")
View(table_mape)

############## 2.USING ses,holt,hw functions ##########################
# Optimum values
# with alpha = 0.2
# Simple Exponential smoothing 
sesa <- ses(train,alpha = 0.2,h=12)
sesa
pred_sesa <- data.frame(predict(sesa,n.ahead=12))
plot(forecast(sesa,n.head=12))
mape_sesa <- MAPE(pred_sesa$Point.Forecast,test)
mape_sesa
# with alpha = 0.2, beta = 0.1
# Holt method
hab <- holt(train,alpha = 0.2,beta = 0.1,h=12)
hab
pred_hab <- data.frame(predict(hab,n.head=12))
plot(forecast(hab,n.head=12))
mape_hab <- MAPE(pred_hab$Point.Forecast,test)
mape_hab
# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# HW Functions
newhwabg <- hw(train,alpha = 0.2, beta = 0.1, gamma = 0.1 ,h=12)
newhwabg
pred_newhwabg <- data.frame(predict(newhwabg,n.head=12,test))
plot(forecast(newhwabg,n.head=12))
mape_newhwabg <- MAPE(pred_newhwabg$Point.Forecast,test)
mape_newhwabg
#  Without optimum values aplha,beta,gamma #

# simple exponential method
# no alpha
sesna <- ses(train,alpha = NULL,h=12)
sesna
pred_sesna <- data.frame(predict(sesna,n.ahead=12))
plot(forecast(sesna,n.head=12))
mape_sesna <- MAPE(pred_sesna$Point.Forecast,test)
mape_sesna
# no alpha, beta 
# Holt method
hnab <- holt(train,alpha = NULL,beta = NULL,h=12)
hnab
pred_hnab <- data.frame(predict(hnab,n.head=12))
plot(forecast(hnab,n.head=12))
mape_hnab <- MAPE(pred_hnab$Point.Forecast,test)
mape_hnab
# no alpha,no beta,no gamma
# HW Functions
newhwnabg <- hw(train,alpha = NULL, beta = NULL, gamma = NULL ,h=12)
newhwnabg
pred_newhwnabg <- data.frame(predict(newhwnabg,n.head=12,test))
plot(forecast(newhwnabg,n.head=12))
mape_newhwnabg <- MAPE(pred_newhwnabg$Point.Forecast,test)
mape_newhwnabg

# MOVING AVERAGE 
mam <- sma(train)
mam
pred_mam <- data.frame(predict(mam,h=12))
plot(forecast(mam,n.head=12))
mape_mam <- MAPE(pred_mam$Point.Forecast,test)
mape_mam

# creating table1 for MAPE
table1_mape <- data.frame(c('mape_sesa','mape_hab','mape_newhwabg','mape_sesna','mape_hnab','mape_newhwnabg','mape_mam'),c(mape_sesa,mape_hab,mape_newhwabg,mape_sesna,mape_hnab,mape_newhwnabg,mape_mam))
colnames(table1_mape) <- c("MAPE","VALUE")
View(table1_mape)


# From 'table' and 'table1' having no alpha,no beta,no gamma with HW Functions( Holt-Winter Additive method) have least MAPE