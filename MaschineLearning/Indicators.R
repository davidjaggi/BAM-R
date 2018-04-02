source("MaschineLearning/Functions.R")

##### Calculate Forece Index ###################################################
#Force Index Indicator
forceindex = (data$Close - data$Open)*data$Vol
forceindex = c(NA,head(forceindex,-1)) ;

##### Calculate Periodic indicator #############################################
# Weekdays as integer
Minute = minute(index(data)); Minute = c(NA, head(Minute,-1));
Hour = hour(index(data)); Hour = c(NA, head(Hour,-1));
Wday = wday(index(data)); Wday = c(NA, head(Wday,-1));
Month = month(index(data)); Month = c(NA, head(Month, -1));

##### Calculate Bollinger Indicator ############################################
Bollinger<-BBands(data[,c("High","Low","Close")],n=20,SMA,sd=2)
BBSqueeze <- as.vector(Bollinger$up-Bollinger$dn)
PcSqueeze <- as.vector(Delt(BBSqueeze, k = 1))
BBSqueeze = c(NA, head(BBSqueeze,-1))
PcSqueeze = c(NA, head(PcSqueeze,-1))
remove(Bollinger)
##### Calculate Aroon indicator ################################################
# Add Aroon indicator
Aroon = aroon(data$High, n = 5); AroonH = c(NA, head(Aroon[,3],-1));
Aroon = aroon(data$Low, n = 5); AroonD = c(NA, head(Aroon[,3], -1));
remove(Aroon)

##### Calculate ATR Indicator ##################################################
#ATR
ATR2 = ATR(data[,c("High","Low","Close")], n=2, matype="WMA")[,1] ; ATR2 = c(NA,head(ATR2,-1)) ;
ATR5 = ATR(data[,c("High","Low","Close")], n=5, matype="WMA")[,1] ; ATR5 = c(NA,head(ATR5,-1)) ;
ATR10 = ATR(data[,c("High","Low","Close")], n=10, matype="WMA")[,1] ; ATR10 = c(NA,head(ATR10,-1)) ;

#ATRIndexes to be inserted

##### Calculate Hurst Indicator ##########################################################
Hurst2 = as.vector(rollapply(return, FUN=HurstK, width=2)); Hurst2 = c(NA,head(Hurst2,-1));
Hurst5 = as.vector(rollapply(return, FUN=HurstK, width=5)); Hurst5 = c(NA,head(Hurst5,-1));
Hurst10 = as.vector(rollapply(return, FUN=HurstK, width=10)); Hurst10 = c(NA,head(Hurst10,-1));
##### Calculate Momentum Indicator #############################################
MOM5 = momentum(data$Close, n = 5, na.pad = TRUE) ; MOM5 = c(NA,head(MOM5, -1));
MOM10 = momentum(data$Close, n = 10, na.pad = TRUE) ; MOM10 = c(NA,head(MOM10, -1)) ;
#MomIndexes to be inserted
##### Calculate Relative Values ################################################
# Difference between High and Close
HC = data$High - data$Close; HC = c(NA, head(HC,-1));

# Difference between Low and Close
CL = data$Close - data$Low; CL = c(NA, head(CL, -1));
PercBody = PercentageBody(data$Open, data$High, data$Low, data$Close); PercBody = c(NA,head(PercBody,-1));
UpWick = UpperWick(data$Open,data$High,data$Low,data$Close); UpWick = c(NA,head(UpWick,-1))
DnWick = DownWick(data$Open,data$High,data$Low,data$Close); DnWick = c(NA,head(DnWick,-1))
RelClose = RelativeClose(data$Open,data$High,data$Low,data$Close); RelClose = c(NA, head(RelClose,-1))
HH5 = as.vector(ifelse(data$Close > runMax(data$High,5), 1, 0)); HH5 = c(NA,head(HH5,-1));
HH10 = as.vector(ifelse(data$Close > runMax(data$High,10), 1, 0)); HH10 = c(NA,head(HH10,-1));
HH15 = as.vector(ifelse(data$Close > runMax(data$High,15), 1, 0)); HH15 = c(NA,head(HH15,-1));
LL5 = as.vector(ifelse(data$Close < runMin(data$Low,5), 1, 0)); LL5 = c(NA,head(LL5,-1));
LL10 = as.vector(ifelse(data$Close < runMin(data$Low,10), 1, 0)); LL10 = c(NA,head(LL10,-1));
LL15 = as.vector(ifelse(data$Close < runMin(data$Low,15), 1, 0)); LL15 = c(NA,head(LL15,-1));
##### Calculate ROC Indocator ##################################################
#Price xhange indicators
ROC5 = ROC(data$Close, n = 5, type = "discrete")*100 ; ROC5 = c(NA,head(ROC5,-1)) ;
ROC10 = ROC(data$Close, n = 10, type = "discrete")*100 ; ROC10 = c(NA,head(ROC10, -1)) ;
##### Calculate RSI Indicator ##################################################
RSI2 = RSI(data$Close, n = 2, matype ="WMA"); RSI2 = c(NA, head(RSI2,-1));
RSI5 = RSI(data$Close, n = 5, matype="WMA"); RSI5 = c(NA,head(RSI5,-1));
RSI10 = RSI(data$Close, n = 10, matype="WMA"); RSI10 = c(NA,head(RSI10,-1));
RSI15 = RSI(data$Close, n = 15, matype="WMA"); RSI15 = c(NA,head(RSI15,-1)); 

##### Calculate Volume Indicators ##############################################
Volume = as.vector(data$Vol); Volume = c(NA,head(Volume,-1))
PcVol = as.vector(Delt(data$Vol, k=1)); PcVol = c(NA, head(PcVol,-1))
##### Calculate Williams Indicator #############################################
#Buy & Sell signal indicators (williams r% and RSI)
willR2 = WPR(data[,c("High","Low","Close")], n=2); willR2 = c(NA,head(willR2,-1));
willR5 = WPR(data[,c("High","Low","Close")], n=5); willR5 = c(NA,head(willR5,-1));
willR10 = WPR(data[,c("High","Low","Close")], n=10); willR10 = c(NA,head(willR10,-1));
willR15 = WPR(data[,c("High","Low","Close")], n=15); willR15 = c(NA,head(willR15,-1));
