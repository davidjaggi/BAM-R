##### Calculate Forece Index ###################################################
#Force Index Indicator
forceindex = (data$Close - data$Open)*data$Vol
forceindex = c(NA,head(forceindex,-1)) ;

##### Calculate Williams Indicator #############################################
#Buy & Sell signal indicators (williams r% and RSI)
willR2 = WPR(data[,c("High","Low","Close")], n=2)
willR2 = c(NA,head(willR2,-1));
willR5 = WPR(data[,c("High","Low","Close")], n=5)
willR5 = c(NA,head(willR5,-1));
willR10 = WPR(data[,c("High","Low","Close")], n=10)
willR5 = c(NA,head(willR10,-1));
willR15 = WPR(data[,c("High","Low","Close")], n=15)
willR5 = c(NA,head(willR15,-1));

##### Calculate RSI Indicator ##################################################
RSI2 = RSI(data$Close, n = 2, matype ="WMA"); RSI2 = c(NA, head(RSI2,-1));
RSI5 = RSI(data$Close, n = 5, matype="WMA"); RSI5 = c(NA,head(RSI5,-1));
RSI10 = RSI(data$Close, n = 10, matype="WMA"); RSI10 = c(NA,head(RSI10,-1));
RSI15 = RSI(data$Close, n = 15, matype="WMA"); RSI15 = c(NA,head(RSI15,-1)); 

##### Calculate ROC Indocator ##################################################
#Price xhange indicators
ROC5 = ROC(data$Close, n = 5, type = "discrete")*100 ; ROC5 = c(NA,head(ROC5,-1)) ;
ROC10 = ROC(data$Close, n = 10, type = "discrete")*100 ; ROC10 = c(NA,head(ROC10, -1)) ;

MOM5 = momentum(data$Close, n = 5, na.pad = TRUE) ; MOM5 = c(NA,head(MOM5, -1)) ;
MOM10 = momentum(data$Close, n = 10, na.pad = TRUE) ; MOM10 = c(NA,head(MOM10, -1)) ;

#MomIndexes to be inserted
##### Calculate ATR Indicator ##################################################
#ATR
ATR2 = ATR(data[,c("High","Low","Close")], n=2, matype="WMA")[,1] ; ATR2 = c(NA,head(ATR2,-1)) ;
ATR5 = ATR(data[,c("High","Low","Close")], n=5, matype="WMA")[,1] ; ATR5 = c(NA,head(ATR5,-1)) ;
ATR10 = ATR(data[,c("High","Low","Close")], n=10, matype="WMA")[,1] ; ATR10 = c(NA,head(ATR10,-1)) ;

#ATRIndexes to be inserted

##### Calculate Relative Values ################################################
# Difference between High and Close
HC = data$High - data$Close; HC = c(NA, head(HC,-1));

# Difference between Low and Close
CL = data$Close - data$Low; CL = c(NA, head(CL, -1));

PercentageBody <- function(Oen,High,Low,Close){
  divisor <- abs(Close-Open)
  dividend <- High-Low
  PercentageBody <- ifelse(dividend > 0, divisor/dividend, 1)
  return(PercentageBody)
}
PercBody = PercentageBody(data$Open, data$High, data$Low, data$Close); PercBody = c(NA,head(PercBody,-1));

UpperWick <- function(Open,High,Low,Close){
  upper <- ifelse(Open >= Close, Open, Close)
  UpperWick <- High-upper
  return(UpperWick)
}
UpWick = UpperWick(data$Open,data$High,data$Low,data$Close); UpWick = c(NA,head(UpWick,-1))

DownWick <- function(Open,High,Low,Close){
  lower <- ifelse(Open <= Close, Open, Close)
  DownWick <- lower-Low
  return(DownWick)
}
DnWick = DownWick(data$Open,data$High,data$Low,data$Close); DnWick = c(NA,head(DnWick,-1))

RelativeClose <- function(Open,High,Low,Close){
  divisor <- Close-Low
  dividend <- High-Low
  RelativeClose <- ifelse(dividend > 0, divisor/dividend, 1)
  return(RelativeClose)
}
RelClose = RelativeClose(data$Open,data$High,data$Low,data$Close); RelClose = c(NA, head(RelClose,-1))
##### Calculate Aroon indicator ################################################
# Add Aroon indicator
Aroon = aroon(data$High, n = 5); AroonH = c(NA, head(Aroon[,3],-1));
Aroon = aroon(data$Low, n = 5); AroonD = c(NA, head(Aroon[,3], -1));

##### Calculate Periodic indicator #############################################
# Weekdays as integer
Wday = wday(index(data)); Wday = c(NA, head(Wday,-1));
Hour = hour(index(data)); Hour = c(NA, head(Hour,-1));
  
