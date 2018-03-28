library(quantmod)

Strategy <- function(x) {
  position <- rep(0,length(x))
  Williams <- x$Williams
  RSI <- x$RSI
  
  for(i in 1:nrow(x)){
    
    if ((Williams[i] >= 0.2) && (Williams[i] >= 0.83)) {
      position[i] <- 1}
    else if ((Williams[i] >= 0.2) && (Williams[i] < 0.83) && (RSI[i] >= 48) && (Williams[i] >= 0.38)) {
      position[i] <- -1} 
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] >= 38 && Williams[i] < 0.38 && RSI[i] >= 60) {
    # position[i] <- 1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] >= 48 && Williams[i] < 0.38 && RSI[i] < 60) {
    # position[i] <- 1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] >= 0.62 && RSI[i] >= 35) {
    # position[i] <- 1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] >= 0.62 && RSI[i] < 35) {
    # position[i] <- 1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] < 0.62) {
    # position[i] <- 1}
    # else (Williams[i] < 0.2) {
    # position[i] <- 1}
  }
  return(position)
}

# Make price series an xts

data$Datetime <- strptime(paste(data$Date, data$Time), "%m/%d/%Y %H:%M")
data <- as.xts(x = data[,3:7], order.by = data$Datetime)

data$Williams <- WPR(data[,c("High","Low","Close")], n=10)
data$RSI <- RSI(data$Close, n = 5, matype="WMA")

data <- na.omit(data)

Position <- Strategy(data)

bhReturns <- Delt(myFX$Close, type = "arithmetic")
myReturns <- bhReturns*Lag(myPosition,1)
myReturns[1] <- 0

Complete <- merge(bhReturns, myReturns)
Complete <- na.omit(Complete)

plot(cumsum(Complete))
