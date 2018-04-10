source("MaschineLearning/Functions.R")

##### Calculate returns and class ##############################################
##compute the price change for the stock ans classify as UP/DOWN
price = as.vector(data$Close-data$Open)
class = as.vector(ifelse(price > 0, "UP","DOWN"))
return = as.vector(Delt(data$Close, k=1))
return2 = as.vector(SumReturns(data = return, n = 2))
return4 = as.vector(SumReturns(data = return, n = 4))
return8 = as.vector(SumReturns(data = return, n = 8))
return12 = as.vector(SumReturns(data = return, n = 12))
return16 = as.vector(SumReturns(data = return, n = 16))
return20 = as.vector(SumReturns(data = return, n = 20))
return24 = as.vector(SumReturns(data = return, n = 24))
##### Calculate Forece Index ###################################################
#Force Index Indicator
forceindex = as.vector((data$Close - data$Open)*data$Vol)

##### Calculate Periodic indicator #############################################
# Weekdays as integer
Minute = minute(index(data))
Hour = hour(index(data))
Wday = wday(index(data))
Month = month(index(data))
DayOfMonth = mday(index(data))

##### Calculate Bollinger Indicator ############################################
Bollinger<-BBands(data[,c("High","Low","Close")],n=20,SMA,sd=2)
BBSqueeze <- as.vector(Bollinger$up-Bollinger$dn)
PcSqueeze <- as.vector(Delt(BBSqueeze, k = 1))
remove(Bollinger)
##### Calculate Aroon indicator ################################################
# Add Aroon indicator
Aroon = aroon(data$High, n = 5); AroonH = as.vector(Aroon[,3])
Aroon = aroon(data$Low, n = 5); AroonD = as.vector(Aroon[,3])
remove(Aroon)

##### Calculate ATR Indicator ##################################################
#ATR
ATR2 = as.vector(ATR(data[,c("High","Low","Close")], n=2, matype="WMA")[,1])
ATR5 = as.vector(ATR(data[,c("High","Low","Close")], n=5, matype="WMA")[,1])
ATR10 = as.vector(ATR(data[,c("High","Low","Close")], n=10, matype="WMA")[,1])

#ATRIndexes to be inserted

##### Calculate Hurst Indicator ##########################################################
Hurst2 = as.vector(rollapply(return, FUN=HurstK, width=2))
Hurst5 = as.vector(rollapply(return, FUN=HurstK, width=5))
Hurst10 = as.vector(rollapply(return, FUN=HurstK, width=10))
##### Calculate Momentum Indicator #############################################
MOM5 = as.vector(momentum(data$Close, n = 5, na.pad = TRUE))
MOM10 = as.vector(momentum(data$Close, n = 10, na.pad = TRUE))

#MomIndexes to be inserted
##### Calculate Relative Values ################################################
# Difference between High and Close
HC = as.vector(data$High - data$Close)

# Difference between Low and Close
CL = as.vector(data$Close - data$Low)
PercBody = as.vector(PercentageBody(data$Open, data$High, data$Low, data$Close))
UpWick = as.vector(UpperWick(data$Open,data$High,data$Low,data$Close))
DnWick = as.vector(DownWick(data$Open,data$High,data$Low,data$Close))
RelClose = as.vector(RelativeClose(data$Open,data$High,data$Low,data$Close))
HH5 = as.vector(ifelse(data$Close > runMax(data$High,5), 1, 0))
HH10 = as.vector(ifelse(data$Close > runMax(data$High,10), 1, 0))
HH15 = as.vector(ifelse(data$Close > runMax(data$High,15), 1, 0))
LL5 = as.vector(ifelse(data$Close < runMin(data$Low,5), 1, 0))
LL10 = as.vector(ifelse(data$Close < runMin(data$Low,10), 1, 0))
LL15 = as.vector(ifelse(data$Close < runMin(data$Low,15), 1, 0))
##### Calculate ROC Indocator ##################################################
#Price xhange indicators
ROC5 = as.vector(ROC(data$Close, n = 5, type = "discrete")*100)
ROC10 = as.vector(ROC(data$Close, n = 10, type = "discrete")*100)
##### Calculate RSI Indicator ##################################################
RSI2 = as.vector(RSI(data$Close, n = 2, matype ="WMA"))
RSI5 = as.vector(RSI(data$Close, n = 5, matype="WMA"))
RSI10 = as.vector(RSI(data$Close, n = 10, matype="WMA"))
RSI15 = as.vector(RSI(data$Close, n = 15, matype="WMA"))

##### Calculate Volume Indicators ##############################################
Volume = as.vector(data$Vol)
PcVol = as.vector(Delt(data$Vol, k=1))
##### Calculate Williams Indicator #############################################
#Buy & Sell signal indicators (williams r% and RSI)
willR2 = as.vector(WPR(data[,c("High","Low","Close")], n=2))
willR5 = as.vector(WPR(data[,c("High","Low","Close")], n=5))
willR10 = as.vector(WPR(data[,c("High","Low","Close")], n=10))
willR15 = as.vector(WPR(data[,c("High","Low","Close")], n=15))
