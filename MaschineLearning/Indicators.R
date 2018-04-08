source("MaschineLearning/Functions.R")

##### Calculate Forece Index ###################################################
#Force Index Indicator
forceindex = (data$Close - data$Open)*data$Vol

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
Aroon = aroon(data$High, n = 5); AroonH = Aroon[,3]
Aroon = aroon(data$Low, n = 5); AroonD = Aroon[,3]
remove(Aroon)

##### Calculate ATR Indicator ##################################################
#ATR
ATR2 = ATR(data[,c("High","Low","Close")], n=2, matype="WMA")[,1]
ATR5 = ATR(data[,c("High","Low","Close")], n=5, matype="WMA")[,1]
ATR10 = ATR(data[,c("High","Low","Close")], n=10, matype="WMA")[,1]

#ATRIndexes to be inserted

##### Calculate Hurst Indicator ##########################################################
Hurst2 = as.vector(rollapply(return, FUN=HurstK, width=2))
Hurst5 = as.vector(rollapply(return, FUN=HurstK, width=5))
Hurst10 = as.vector(rollapply(return, FUN=HurstK, width=10))
##### Calculate Momentum Indicator #############################################
MOM5 = momentum(data$Close, n = 5, na.pad = TRUE)
MOM10 = momentum(data$Close, n = 10, na.pad = TRUE)
#MomIndexes to be inserted
##### Calculate Relative Values ################################################
# Difference between High and Close
HC = data$High - data$Close

# Difference between Low and Close
CL = data$Close - data$Low
PercBody = PercentageBody(data$Open, data$High, data$Low, data$Close)
UpWick = UpperWick(data$Open,data$High,data$Low,data$Close)
DnWick = DownWick(data$Open,data$High,data$Low,data$Close)
RelClose = RelativeClose(data$Open,data$High,data$Low,data$Close)
HH5 = as.vector(ifelse(data$Close > runMax(data$High,5), 1, 0))
HH10 = as.vector(ifelse(data$Close > runMax(data$High,10), 1, 0))
HH15 = as.vector(ifelse(data$Close > runMax(data$High,15), 1, 0))
LL5 = as.vector(ifelse(data$Close < runMin(data$Low,5), 1, 0))
LL10 = as.vector(ifelse(data$Close < runMin(data$Low,10), 1, 0))
LL15 = as.vector(ifelse(data$Close < runMin(data$Low,15), 1, 0))
##### Calculate ROC Indocator ##################################################
#Price xhange indicators
ROC5 = ROC(data$Close, n = 5, type = "discrete")*100
ROC10 = ROC(data$Close, n = 10, type = "discrete")*100
##### Calculate RSI Indicator ##################################################
RSI2 = RSI(data$Close, n = 2, matype ="WMA")
RSI5 = RSI(data$Close, n = 5, matype="WMA")
RSI10 = RSI(data$Close, n = 10, matype="WMA")
RSI15 = RSI(data$Close, n = 15, matype="WMA") 

##### Calculate Volume Indicators ##############################################
Volume = as.vector(data$Vol)
PcVol = as.vector(Delt(data$Vol, k=1))
##### Calculate Williams Indicator #############################################
#Buy & Sell signal indicators (williams r% and RSI)
willR2 = WPR(data[,c("High","Low","Close")], n=2)
willR5 = WPR(data[,c("High","Low","Close")], n=5)
willR10 = WPR(data[,c("High","Low","Close")], n=10)
willR15 = WPR(data[,c("High","Low","Close")], n=15)
