library(readr)
library(readxl)
library(forecast)
library(fpp)
library(smooth)
library(tseries)
cola <- read_xlsx('D:/WORK/FORECASTING/CocaCola_Sales_Rawdata.xlsx')
View(cola)
#ggplot
library(ggplot2)
ggplot(data = cola,aes(x =cola$Quarter,y = cola$Sales)) + 
  geom_point(color='blue') + geom_line(color='red')
c <- ts(cola$Sales,frequency = 4,start=c(86))
View(c)
train<-c[1:38]
test<-c[39:42]
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)
plot(c)
#### 1.USING HoltWinters function ################
# Optimum values
# with alpha = 0.2
hwa <- HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hwa
pred_hwa <- forecast(hwa)
pred_hwa <- data.frame(predict(hwa,n.ahead = 4))
plot(forecast(hwa,h=4))
mape_hwa <- MAPE(pred_hwa$fit,test)*100
mape_hwa
# with alpha = 0.2, beta = 0.1
hwab <- HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hwab
pred_hwab <- forecast(hwab)
pred_hwab <- data.frame(predict(hwab,n.ahead = 4))
plot(forecast(hwab,h=4))
mape_hwab <- MAPE(pred_hwab$fit,test)*100
mape_hwab
# with alpha = 0.2, beta = 0.1, gamma = 0.1 
hwabg <- HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hwabg
pred_hwabg <- forecast(hwabg)
pred_hwabg <- data.frame(predict(hwabg,n.ahead = 4))
plot(forecast(hwabg,h=4))
mape_hwabg <- MAPE(pred_hwabg$fit,test)*100
mape_hwabg
# Without optimum values aplha,beta,gamma #
# no alpha
hwna <- HoltWinters(train,beta = F,gamma = F)
hwna
pred_hwna <- forecast(hwna)
pred_hwna <- data.frame(predict(hwna,n.ahead = 4))
plot(forecast(hwna,h=4))
mape_hwna <- MAPE(pred_hwna$fit,test)*100
mape_hwna
# no alpha,no beta
hwnab <- HoltWinters(train,gamma = F)
hwnab
pred_hwnab <- forecast(hwnab)
pred_hwnab <- data.frame(predict(hwnab,n.ahead = 4))
plot(forecast(hwnab,h=4))
mape_hwnab <- MAPE(pred_hwnab$fit,test)*100
mape_hwnab
# no alpha,no beta,no gamma
hwnabg <- HoltWinters(train)
hwnabg
pred_hwnabg <- forecast(hwnabg)
pred_hwnabg <- data.frame(predict(hwnabg,n.ahead = 4))
plot(forecast(hwnabg,h=4))
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
sesa <- ses(train,alpha = 0.2,h=4)
sesa
pred_sesa <- data.frame(predict(sesa,n.ahead=4))
plot(forecast(sesa,n.head=4))
mape_sesa <- MAPE(pred_sesa$Point.Forecast,test)
mape_sesa
# with alpha = 0.2, beta = 0.1
# Holt method
hab <- holt(train,alpha = 0.2,beta = 0.1,h=4)
hab
pred_hab <- data.frame(predict(hab,n.head=4))
plot(forecast(hab,n.head=4))
mape_hab <- MAPE(pred_hab$Point.Forecast,test)
mape_hab

# with alpha = 0.2, beta = 0.1, gamma = 0.1 

# HW Functions
newhwabg <- hw(train,alpha = 0.2, beta = 0.1, gamma = 0.1 ,h=4)
newhwabg
pred_newhwabg <- data.frame(predict(newhwabg,n.head=4,test))
plot(forecast(newhwabg,n.head=4))
mape_newhwabg <- MAPE(pred_newhwabg$Point.Forecast,test)
mape_newhwabg
#  Without optimum values aplha,beta,gamma #

# simple exponential method
# no alpha
sesna <- ses(train,alpha = NULL,h=4)
sesna
pred_sesna <- data.frame(predict(sesna,n.ahead=4))
plot(forecast(sesna,n.head=4))
mape_sesna <- MAPE(pred_sesna$Point.Forecast,test)
mape_sesna
# no alpha, beta 
# Holt method
hnab <- holt(train,alpha = NULL,beta = NULL,h=4)
hnab
pred_hnab <- data.frame(predict(hnab,n.head=4))
plot(forecast(hnab,n.head=4))
mape_hnab <- MAPE(pred_hnab$Point.Forecast,test)
mape_hnab
# no alpha,no beta,no gamma
# HW Functions
newhwnabg <- hw(train,alpha = NULL, beta = NULL, gamma = NULL ,h=4)
newhwnabg
pred_newhwnabg <- data.frame(predict(newhwnabg,n.head=4,test))
plot(forecast(newhwnabg,n.head=4))
mape_newhwnabg <- MAPE(pred_newhwnabg$Point.Forecast,test)
mape_newhwnabg

# MOVING AVERAGE 
mam <- sma(train)
mam
pred_mam <- data.frame(predict(mam,h=4))
plot(forecast(mam,n.head=4))
mape_mam <- MAPE(pred_mam$Point.Forecast,test)
mape_mam

# creating table1 for MAPE
table1_mape <- data.frame(c('mape_sesa','mape_hab','mape_newhwabg','mape_sesna','mape_hnab','mape_newhwnabg','mape_mam'),c(mape_sesa,mape_hab,mape_newhwabg,mape_sesna,mape_hnab,mape_newhwnabg,mape_mam))
colnames(table1_mape) <- c("MAPE","VALUE")
View(table1_mape)

# From 'table' and 'table1' having no alpha,no beta,no gamma with HW Functions( Holt-Winter Additive method) have least MAPE
