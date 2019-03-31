#Business understanding:
#Global Mart is an online store super giant having worldwide operations. It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office.
#Now as a sales/operations manager, you want to finalise the plan for the next 6 months.  So, you want to forecast the sales and the demand for the next 6 months, that would help you manage the revenue and inventory accordingly.

## Data understanding:
#The data has 24 attributes related to each such transaction. The Market attribute has 7-factor levels representing the geographical 
#market sector that the customer belongs to. The Segment attribute tells which of the 3 segments that customer belongs to


## Data preparation:

#You would need to first segment the whole dataset into the 21 subsets based on the market and the customer segment level. 
#Next to convert the transaction-level data into a time series.

#aggregate the 3 attributes  - Sales, Quantity & Profit, over the Order Date to arrive at monthly values for these attributes
#arrive at these 3 time series for each of the 21 segments, we need to find the 2 most profitable and consistently profitable 
#segments. For this, the metric that you can use is the coefficient of variation of the Profit for all 21 market segments

## Model building(Objective):
#1. the next challenge is to forecast the sales and quantity for the next 6 months.
#2. use classical decomposition and auto ARIMA for forecasting.Smoothen the data before you perform classical decomposition.

# Model evaluation:
#next step would be to forecast the sales/demand for next 6 months using this model. To test the accuracy of your forecast, 
#you must initially separate out the last 6 months values from your dataset, after aggregating the transaction level data into
#the monthly data. Then check your 6 months forecast using the out-of-sample figures. You can use MAPE for this.


############## Import the data

GlobalStoreData<-read.csv("Global Superstore.csv")

# Load libraries..
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(forecast)
library(tseries)
require(graphics)

#structure of Data.
str(GlobalStoreData) ## contains categorical variable(important cols Market and Segment and Order date) and numerical variable(important cols- Sales,Quantity, Profit)
### No consistent datatype for continous type.

summary(GlobalStoreData)
## Continous date-  Profit/Sales/Quatity must have  outliers as the 75% data and max data have huge gap.. Na's in postal code.

## Lets check for null data..
nrow(GlobalStoreData)
## 41296 are null values total cols =51290 => 41296/51290= 80% ## since postal code is not a very important factor for Sales forcasting hence
#3 Lets drop this column off
drops <- c("Postal.Code")
GlobalStoreData<-GlobalStoreData[ , !(names(GlobalStoreData) %in% drops)]
sum(duplicated(GlobalStoreData)) ## No duplicate enteries found..

## Convert columns into proper datatypes.
GlobalStoreData$Ship.Date=strptime(as.character(GlobalStoreData$Ship.Date), "%d-%m-%Y")
GlobalStoreData$Order.Date=strptime(as.character(GlobalStoreData$Order.Date), "%d-%m-%Y")
GlobalStoreData$Ship.Date<-as.Date(GlobalStoreData$Ship.Date,"%d-%m-%Y")
GlobalStoreData$Order.Date<-format(as.Date(GlobalStoreData$Order.Date), "%Y-%m")



####################### DATA  PREPERATION #######################################################
# Lets subset the  data based into geographical location and market segment.
#Aggregating market buckets, order date by sales, quantity & profit
#Aggregating data by Sales, Profit & Quantity & coefficientOfVariation
GlobalStoreData_aggregated <- GlobalStoreData %>%  group_by(Market) %>% summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)*100/mean(Profit))            

#Aggregating data monthly on each attribute
GlobalStoreData_aggregated_monthly <- GlobalStoreData %>% group_by(Market, Segment,Order.Date) %>% summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)*100/mean(Profit))
ProfitBysales <- GlobalStoreData[,c("Profit","Sales","Market","Segment","Quantity")] %>% group_by(Market,Segment) %>% dplyr::summarise(., sum(Sales),sum(Profit),sd(Profit)*100/mean(Profit))
colnames(ProfitBysales) = c("Market","Segment","Sales","Profit","coefficientOfVariation")
View(ProfitBysales)
ggplot(ProfitBysales, aes(Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(ProfitBysales, aes(Segment, Profit, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(ProfitBysales, aes(Segment, coefficientOfVariation, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
#EU and APAC Consumer have lowest coefficientOfVariation and highest Profit. 
#Even EU,APAC markets are quiet consistent in their profit across segments


##################Model building##############################
#---------------------------------------------------------Model Building------------------------------------------------------------------
#Subsetting data_aggregated_monthly for top 2 market segments
GlobalStoreDataTop2Segments <- subset(GlobalStoreData_aggregated_monthly, ((Market == "EU" | Market == "APAC")&Segment=="Consumer"))
str(GlobalStoreDataTop2Segments)

#Changing column names
names(GlobalStoreDataTop2Segments) <- c("Market", "Segment","Order_Month", "Sales", "Profit", "Quantity", "CV")


# selecting "Sales", "Profit", "Quantity" columns.
APAC_Consumer_Agg <- GlobalStoreDataTop2Segments[which(GlobalStoreDataTop2Segments$Market=="APAC"),][,c(3:6)]
EU_Consumer_Agg <- GlobalStoreDataTop2Segments[which(GlobalStoreDataTop2Segments$Market=="EU"),][,c(3:6)]


###--------------------------------------------------APAC Consumer Sales Forecast--------------------------------------------
#Creating APAC sales timeseries
APAC_Consumer_Sales_TS <- ts(APAC_Consumer_Agg$Sales,frequency=12,start=c(2011,1),end=c(2014,12))

plot(APAC_Consumer_Sales_TS,xlab="Years",ylab="Sales")
##Quite fluctuations in sales from 2011 to 2015
autoplot(APAC_Consumer_Sales_TS) + labs(x ="Year", y = "Sales", title="Sales in APAC Consumer Region") + theme_classic()

boxplot(APAC_Consumer_Sales_TS~cycle(APAC_Consumer_Sales_TS),xlab="Year", ylab = "Sales in APAC Consumer Region" ,main ="Monthly Sales from 2011 to 2014")
## Not much outlier found
decompose_APAC_Consumer_Sales_TS <- decompose(APAC_Consumer_Sales_TS,"multiplicative")
dev.off()
autoplot(decompose_APAC_Consumer_Sales_TS) + theme_classic()## trend is decreasing and quite seasonality is found in data.


cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APAC_Consumer_Sales_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  plot(smoothedseries,xlab="Years",ylab="Sales")
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)
smoothedseries <- HoltWinters(APAC_Consumer_Sales_TS, alpha=.3,beta=FALSE, gamma=FALSE)

#Smoothing time series using Moving Average method
w <- 4
APAC_Consumer_Sales_Smoothed <- stats::filter(APAC_Consumer_Sales_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- APAC_Consumer_Sales_Smoothed[w+2] - APAC_Consumer_Sales_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_Consumer_Sales_Smoothed[i] <- APAC_Consumer_Sales_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(APAC_Consumer_Sales_Smoothed)
diff <- APAC_Consumer_Sales_Smoothed[n-w] - APAC_Consumer_Sales_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Consumer_Sales_Smoothed[i] <- APAC_Consumer_Sales_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
APAC_Consumer_Sales_df <- data.frame(cbind(APAC_Consumer_Agg$Order_Month,APAC_Consumer_Sales_Smoothed))
colnames(APAC_Consumer_Sales_df) <- c("Month","Sales")

#Changing Sales type
APAC_Consumer_Sales_df$Sales <- as.numeric(as.character((APAC_Consumer_Sales_df$Sales)))

#Plotting smoothed series
plot(APAC_Consumer_Sales_TS)
lines(APAC_Consumer_Sales_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='blue',lwd=2)

#Again creating time series from dataframe
APAC_Consumer_Sales_TS <- ts(APAC_Consumer_Agg$Sales,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(APAC_Consumer_Sales_TS)-ntest
train.ts <- window(APAC_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain))
validation.ts <- window(APAC_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(APAC_Consumer_Agg$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
# Adjusted R-squared:  0.7224
#Lets forcast the predictable part
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(APAC_Consumer_Sales_Smoothed,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red",title=c("APAC Consumer Sales"))  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
## the forcasted values are quite close to original series.
#Checking MAPE for our linear model
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,validation.ts)  
APAC_consumer_accuracy #MAPE=13.93   its quiet low hense the the variance from original 
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
graphics.off()
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=17.68447 for Training set & 11.77618 for Validation set for Auto ARIMA Quiet acceptable and
# not much difff in test and train hense not much overfitting.

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(APAC_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#Again, let's check if the residual series is white noise

resi_auto_arima <- APAC_Consumer_Sales_TS - fitted(autoarima_ts)

adf.test(resi_auto_arima,alternative = "stationary")
##3 p value>.05.. reject null hypothesis.. and accept alternative hypothesis.. series is stationary
kpss.test(resi_auto_arima)
# pvalue > .05  ### accept  null hypothesis.. the series is stationary
######################################################################################################
###--------------------------------------------------APAC Consumer Quantity Forecast--------------------------------------------
#Lets create  APAC Quantity timeseries
APAC_Consumer_Quantity_TS <- ts(APAC_Consumer_Agg$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))


plot(APAC_Consumer_Quantity_TS,xlab="Years",ylab="Sales")
##Salescertainly ahve trend and seasonality.
autoplot(APAC_Consumer_Quantity_TS) + labs(x ="Year", y = "Sales", title="Sales in APAC Consumer Region") + theme_classic()

boxplot(APAC_Consumer_Quantity_TS~cycle(APAC_Consumer_Quantity_TS),xlab="Year", ylab = "Sales in APAC Consumer Region" ,main ="Monthly Sales from 2011 to 2014")
## Not much outlier found.. sales values differs quite often..
decompose_APAC_Consumer_Quantity_TS <- decompose(APAC_Consumer_Quantity_TS,"multiplicative")
autoplot(decompose_APAC_Consumer_Quantity_TS) + theme_classic()## trend is decreasing and quite seasonality is found in data.


### Increase trend in Quantity
#Testing Holt winters for Smoothing
plot(APAC_Consumer_Quantity_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(APAC_Consumer_Quantity_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method
w <- 4
APAC_Consumer_Quantity_Smoothed <- stats::filter(APAC_Consumer_Quantity_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- APAC_Consumer_Quantity_Smoothed[w+2] - APAC_Consumer_Quantity_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_Consumer_Quantity_Smoothed[i] <- APAC_Consumer_Quantity_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(APAC_Consumer_Quantity_Smoothed)
diff <- APAC_Consumer_Quantity_Smoothed[n-w] - APAC_Consumer_Quantity_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Consumer_Quantity_Smoothed[i] <- APAC_Consumer_Quantity_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
APAC_Consumer_Quantity_df <- data.frame(cbind(APAC_Consumer_Agg$Order_Month,APAC_Consumer_Quantity_Smoothed))
colnames(APAC_Consumer_Quantity_df) <- c("Month","Quantity")

#Changing Quantity type
APAC_Consumer_Quantity_df$Quantity <- as.numeric(as.character((APAC_Consumer_Quantity_df$Quantity)))

#Plotting smoothed series
plot(APAC_Consumer_Quantity_TS)
lines(APAC_Consumer_Quantity_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='blue',lwd=2)

#Again creating time series from dataframe
APAC_Consumer_Quantity_TS <- ts(APAC_Consumer_Quantity_df$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6  ## forcast for 6 months
nTrain <- length(APAC_Consumer_Quantity_TS)-ntest
train.ts <- window(APAC_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
validation.ts <- window(APAC_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(APAC_Consumer_Quantity_df$Quantity,frequency=12,start=c(2015,1),end=c(2015,6)) ## test for 6 months

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(APAC_Consumer_Quantity_Smoothed,ylab="Quantity",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
#Checking MAPE for our linear model
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,validation.ts)  
APAC_consumer_accuracy #MAPE=9.580049
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,validation.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=4.80180 for Training set & 10.36941 for Validation set for Auto ARIMA ## no overfitting..

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Regresssion model
train.lm.model <- tslm(APAC_Consumer_Quantity_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")

#Again, let's check if the residual series is white noise

resi_auto_arima <- APAC_Consumer_Quantity_TS - fitted(train.lm.model)

adf.test(resi_auto_arima,alternative = "stationary")
##3 p value>.05.. reject null hypothesis.. and accept alternative hypothesis.. series is stationary
kpss.test(resi_auto_arima)

########################################################################################################################
###--------------------------------------------------EU Consumer Sales Forecast--------------------------------------------
#Creating EU Sales timeseries
EU_Consumer_Sales_TS <- ts(EU_Consumer_Agg$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Sales_TS,xlab="Years",ylab="Sales")
##Quite fluctuations in sales from 2011 to 2015
autoplot(EU_Consumer_Sales_TS) + labs(x ="Year", y = "Sales", title="Sales in EU Consumer Region") + theme_classic()

boxplot(EU_Consumer_Sales_TS~cycle(EU_Consumer_Sales_TS),xlab="Year", ylab = "Sales in EU Consumer Region" ,main ="Monthly Sales from 2011 to 2014")
## Not much outlier found
decompose_EU_Consumer_Sales_TS <- decompose(EU_Consumer_Sales_TS,"multiplicative")
dev.off()
autoplot(decompose_EU_Consumer_Sales_TS) + theme_classic()## trend is decreasing and quite seasonality is found in data.
#### trend na dseasonality observed...


#Testing Holt winters for Smoothing
plot(EU_Consumer_Sales_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EU_Consumer_Sales_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method
w <- 5
EU_Consumer_Sales_Smoothed <- stats::filter(EU_Consumer_Sales_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- EU_Consumer_Sales_Smoothed[w+2] - EU_Consumer_Sales_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_Consumer_Sales_Smoothed[i] <- EU_Consumer_Sales_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(EU_Consumer_Sales_Smoothed)
diff <- EU_Consumer_Sales_Smoothed[n-w] - EU_Consumer_Sales_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_Sales_Smoothed[i] <- EU_Consumer_Sales_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
EU_Consumer_Sales_df <- data.frame(cbind(EU_Consumer_Agg$Order_Month,EU_Consumer_Sales_Smoothed))
colnames(EU_Consumer_Sales_df) <- c("Month","Sales")

#Changing Sales type
EU_Consumer_Sales_df$Sales <- as.numeric(as.character((EU_Consumer_Sales_df$Sales)))

#Plotting smoothed series
plot(EU_Consumer_Sales_TS)
lines(EU_Consumer_Sales_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='blue',lwd=2)

#Again creating time series from dataframe
EU_Consumer_Sales_TS <- ts(EU_Consumer_Sales_df$Sales,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Sales_TS)-ntest
train.ts <- window(EU_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Sales_df$Sales,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_Consumer_Sales_Smoothed,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
### not much diff b/w forcasted and original series.
#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=13.80817
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=5.098584 for Training set & 3.343440 for validation set for Auto ARIMA
## less variance..

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#Again, let's check if the residual series is white noise

resi_auto_arima <- EU_Consumer_Sales_TS - fitted(autoarima_ts)

adf.test(resi_auto_arima,alternative = "stationary")
##3 p value>.05.. reject null hypothesis.. and accept alternative hypothesis.. series is stationary
kpss.test(resi_auto_arima)
###--------------------------------------------------EU Consumer Quantity Forecast--------------------------------------------
#Creating EU Quantity timeseries
EU_Consumer_Quantity_TS <- ts(EU_Consumer_Agg$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Quantity_TS)

plot(EU_Consumer_Quantity_TS,xlab="Years",ylab="Sales")
##Queit fluctuations in sales from 2011 to 2015
autoplot(EU_Consumer_Quantity_TS) + labs(x ="Year", y = "Sales", title="Sales in APAC Consumer Region") + theme_classic()

boxplot(EU_Consumer_Quantity_TS~cycle(EU_Consumer_Quantity_TS),xlab="Year", ylab = "Sales in APAC Consumer Region" ,main ="Monthly Sales from 2011 to 2014")
## Not much outlier found
decompose_EU_Consumer_Quantity_TS <- decompose(EU_Consumer_Quantity_TS,"multiplicative")
autoplot(decompose_EU_Consumer_Quantity_TS) + theme_classic()## trend is decreasing and quite seasonality is found in data.


#Testing Holt winters for Smoothing
plot(EU_Consumer_Quantity_TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.2, 0.99, 0.56)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(EU_Consumer_Quantity_TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Smoothing time series using Moving Average method
w <- 3
EU_Consumer_Quantity_Smoothed <- stats::filter(EU_Consumer_Quantity_TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- EU_Consumer_Quantity_Smoothed[w+2] - EU_Consumer_Quantity_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_Consumer_Quantity_Smoothed[i] <- EU_Consumer_Quantity_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(EU_Consumer_Quantity_Smoothed)
diff <- EU_Consumer_Quantity_Smoothed[n-w] - EU_Consumer_Quantity_Smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_Quantity_Smoothed[i] <- EU_Consumer_Quantity_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame with Order_Month & renaming columns
#Using moving average smoothed time series here
EU_Consumer_Quantity_df <- data.frame(cbind(EU_Consumer_Agg$Order_Month,EU_Consumer_Quantity_Smoothed))
colnames(EU_Consumer_Quantity_df) <- c("Month","Quantity")

#Changing Quantity type
EU_Consumer_Quantity_df$Quantity <- as.numeric(as.character((EU_Consumer_Quantity_df$Quantity)))

#Plotting smoothed series
plot(EU_Consumer_Quantity_TS)
lines(EU_Consumer_Quantity_Smoothed,col='blue',lwd=2)
lines(fitted(smoothedseries)[,1],col='blue',lwd=2)

#Again creating time series from dataframe
EU_Consumer_Quantity_TS <- ts(EU_Consumer_Quantity_df$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Quantity_TS)-ntest
train.ts <- window(EU_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Quantity_df$Quantity,frequency=12,start=c(2015,1),end=c(2015,6))

#Model 1: Regression model based on trend & seasonality sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_Consumer_Quantity_Smoothed,ylab="Quantity",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=13.72469
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=6.244317 for Training set & 19.685536 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Quantity_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto Regression model
train.lm.model <- tslm(EU_Consumer_Quantity_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")

#Again, let's check if the residual series is white noise

resi_auto_arima <- EU_Consumer_Quantity_TS - fitted(train.lm.model)

adf.test(resi_auto_arima,alternative = "stationary")
##3 p value>.05.. reject null hypothesis.. and accept alternative hypothesis.. series is stationary
kpss.test(resi_auto_arima)

