library(quantmod)

Strategy <- function(data) {
  position <- rep(0,nrow(data))
  Williams <- data$Williams
  RSI <- data$RSI
  
  for(i in 1:nrow(data)){
    
    if ((Williams[i] >= 0.2) && (Williams[i] >= 0.83)) {
      position[i] <- 1}
    else if ((Williams[i] >= 0.2) && (Williams[i] < 0.83) && (RSI[i] >= 48) && (Williams[i] >= 0.38)) {
      position[i] <- -1} 
    else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] >= 38 && Williams[i] < 0.38 && RSI[i] >= 60) {
      position[i] <- 1}
    else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] >= 48 && Williams[i] < 0.38 && RSI[i] < 60) {
      position[i] <- 1}
    else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] >= 0.62 && RSI[i] >= 35) {
      position[i] <- 1}
    else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] >= 0.62 && RSI[i] < 35) {
      position[i] <- 1}
    else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] < 0.62) {
      position[i] <- 1}
    else if(Williams[i] < 0.2) {
     position[i] <- 1}
  }
  return(position)
}

# Make price series an xts

data$Datetime <- strptime(paste(data$Date, data$Time), "%m/%d/%Y %H:%M")
data <- as.xts(x = data[,3:7], order.by = data$Datetime)

data$Williams <- WPR(data[,c("High","Low","Close")], n=10)
data$RSI <- RSI(data$Close, n = 5, matype="WMA")

data <- na.omit(data)

data$Position <- Strategy(data)

bhReturns <- Delt(data$Close, type = "arithmetic")
myReturns <- bhReturns*Lag(data$Position,1)
myReturns[1] <- 0

complete <- merge(bhReturns, myReturns)
complete <- na.omit(complete)

plot(cumsum(complete))
