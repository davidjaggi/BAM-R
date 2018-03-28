library(quantmod)

Strategy <- function(x) {
  Williams <- WPR(x[,c("High","Low","Close")], n=10)
  RSI <- RSI(x$Close, n = 5, matype="WMA")
  RSI5 = RSI(data$Close, n = 5, matype="WMA");
  position <- rep(0,length(x))
  for(i in 1:length(x)){
    if (Williams[i] >= 0.2 & Williams[i] >= 0.83) {
      position[i] <- 1} else {position <- 0}
    # else if (williams >= 0.2 & williams < 0.83 & RSI >= 48 & williams >= 0.38) {
    #   position <- -1} 
    # else if(williams >= 0.2 & williams < 0.83 & RSI >= 38 & williams < 0.38 & RSI >= 60) {
    #   position <- 1}
    # else if(williams >= 0.2 & williams < 0.83 & RSI >= 48 & williams < 0.38 & RSI < 60) {
    #   position <- 1}
    # else if(williams >= 0.2 & williams < 0.83 & RSI < 48 & williams >= 0.62 & RSI >= 35) {
    #   position <- 1}
    # else if(williams >= 0.2 & williams < 0.83 & RSI < 48 & williams >= 0.62 & RSI < 35) {
    #   position <- 1}
    # else if(williams >= 0.2 & williams < 0.83 & RSI < 48 & williams < 0.62) {
    #   position <- 1}
    # else (williams < 0.2) {
    #   position <- 1}
  }
  return(position)
}

# Make price series an xts

data$Datetime <- strptime(paste(data$Date, data$Time), "%m/%d/%Y %H:%M")
data <- as.xts(x = data[,3:7], order.by = data$Datetime)

myFX <- data

myPosition <- Strategy(myFX)

bhReturns <- Delt(myFX$Close, type = "arithmetic")
myReturns <- bhReturns*Lag(myPosition,1)
myReturns[1] <- 0

Complete <- merge(bhReturns, myReturns)
Complete <- na.omit(Complete)

plot(cumsum(Complete))
