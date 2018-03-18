library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(forecast)
library(stringr)
library(tseries)
library(TStools)
library(thief)

Yahoo=fread('Yahoo.txt')
Yahoo=as.data.frame(Yahoo)
Yahoo$TIME=mdy_hms(Yahoo$TIME)

Yahoo %>%
  group_by(PHASE_ID) %>% 
  summarise(count=n()) %>%
  arrange(-count)

PHASE_ID_24=Yahoo %>%
  filter(PHASE_ID==24) %>%
  group_by(TIME) %>%
  summarise(Price=max(PRICE))

ggplot(PHASE_ID_24,aes(x=TIME,y=Price))+
  geom_line(stat='identity')

write.csv(PHASE_ID_24,'Yahoo_TS_data.csv')

### Frequency
frequency=PHASE_ID_24 %>%
  filter(TIME>=as.POSIXct("2002-07-18 00:00:00", tz='UTC')) %>%
  filter(TIME<=as.POSIXct("2002-07-18 14:00:00", tz='UTC'))

ggplot(frequency,aes(x=TIME,y=Price))+
  geom_line(stat='identity')

Frequency=PHASE_ID_24 %>%
  filter(TIME>=as.POSIXct("2002-07-18 00:00:00", tz='UTC')) %>%
  filter(TIME<=as.POSIXct("2002-07-25 00:00:00", tz='UTC'))

ggplot(Frequency,aes(x=TIME,y=Price))+
  geom_line(stat='identity')

project=PHASE_ID_24$Price
project=as.data.frame(project)
colnames(project)='Price'

# Time series data
plot(project,type='l')
project=ts(project, frequency = 28)

train=window(project, start=1, end=650)
test=window(project,start=651, end=674)

plot(train,type='l')

plot(test,type='l')

######## ARIMA ###########
Acf(train)
tsdisplay(train)
ndiffs(train) # nonseasonal unstable
nsdiffs(train) # seasonal stable

train_diff=diff(train,lag=1,differences = 1)
plot(train_diff,type='l')
tsdisplay(train_diff)

train_diff_1=diff(train,lag=28,differences =1)
plot(train_diff_1)
tsdisplay(train_diff_1)

auto=auto.arima(train)
auto
tsdisplay(resid(auto))
Box.test(resid(auto))
qqnorm(resid(auto))
qqline(resid(auto))

auto_f=forecast(auto,h=673)
plot(auto_f)
accuracy(auto_f,test)

testa=auto_f$mean
plot(testa, type='l')


model_arima=Arima(train,order=c(0,1,1),seasonal = c(0,0,0))
model_arima
tsdisplay(resid(model_arima))
Box.test(resid(model_arima))
qqnorm(resid(model_arima))
qqline(resid(model_arima))
accuracy(model_arima)

model_arima_f=forecast(model_arima,h=673)
model_arima_f
plot(model_arima_f)
accuracy(model_arima_f,test)

######## Smoothing ###########

## SES
model_SE=HoltWinters(train,beta=FALSE,gamma=FALSE)
plot(model_SE)
sqrt(model_SE$SSE/11173)
model_SE

model_SE_f=forecast(model_SE,h=7645)
plot(model_SE_f)
accuracy(model_SE_f,test)

## Holt
model_holt=HoltWinters(train,gamma=FALSE)
model_holt
sqrt(model_holt$SSE/11173)
plot(model_holt)

model_holt_f=forecast(model_holt,h=7645)
plot(model_holt_f)
accuracy(model_holt_f,test)

# Holt with addictive seasonality
?HoltWinters
model_holt_add=HoltWinters(train,seasonal = 'additive')
sqrt(model_holt_add$SSE/18173)
plot(model_holt_add)

model_holt_add_f=forecast(model_holt_add,h=673)
plot(model_holt_add_f)
accuracy(model_holt_add_f,test)

testa=model_holt_add_f$mean
plot(testa,type='l')

# Holt with multiplicative seasonality
model_holt_multi=HoltWinters(train,seasonal = 'multiplicative')
sqrt(model_holt_multi$SSE/18173)
plot(model_holt_multi)

model_holt_multi_f=forecast(model_holt_multi,h=673)
plot(model_holt_multi_f)
accuracy(model_holt_multi_f,test)

testm=model_holt_multi_f$mean
plot(testm,type='l')

######## Decomposition ###########
model_decomp=decompose(train)
plot(model_decomp)

model_decomp_f=forecast(model_decomp$x,h=673)
plot(model_decomp_f)
model_decomp_f$mean
accuracy(model_decomp_f,test)

testd=model_decomp_f$mean
plot(testd,type='l')

######## Neural Network ###########
?nnetar
model_nn=nnetar(train)
model_nn
accuracy(model_nn)
model_nn_f=forecast(model_nn,h=673)
plot(model_nn_f)
accuracy(model_nn_f,test)

testn=model_nn_f$mean
plot(testn,type='l')



