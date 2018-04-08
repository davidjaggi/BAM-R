library(quantmod)
library(PerformanceAnalytics)

Strategy <- function(data) {
  position <- rep(0,nrow(data))
  DayOfMonth <- data$DayOfMonth
  Hour <- data$Hour

  for(i in 1:nrow(data)){
    
    # if ((Williams[i] >= 0.2) && (Williams[i] >= 0.83)) {
    #   position[i] <- 1}
    # else if ((Williams[i] >= 0.2) && (Williams[i] < 0.83) && (RSI[i] >= 48) && (Williams[i] >= 0.38)) {
    #   position[i] <- -1} 
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] >= 38 && Williams[i] < 0.38 && RSI[i] >= 60) {
    #   position[i] <- -1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] >= 48 && Williams[i] < 0.38 && RSI[i] < 60) {
    #   position[i] <- -1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] >= 0.62 && RSI[i] >= 35) {
    #   position[i] <- 1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] >= 0.62 && RSI[i] < 35) {
    #   position[i] <- -1}
    # else if(Williams[i] >= 0.2 && Williams[i] < 0.83 && RSI[i] < 48 && Williams[i] < 0.62) {
    #   position[i] <- 1}
    # else if(Williams[i] < 0.2) {
    #  position[i] <- 1}
    ifelse(DayOfMonth[i] <= 10, ifelse((Hour >= 12) && (Hour <= 19), position[i] <- -1, position[i] <- 0),position[i] <- 0)
  }
  return(position)
}

# Make price series an xts
data$DayOfMonth <- DayOfMonth
data$Hour <- Hour

data$Position <- Strategy(data)

myReturns <- return*Lag(data$Position,1)
myReturns[1] <- 0

complete <- merge(return, myReturns)
complete <- na.omit(complete)

plot(cumsum(complete))
# table.Drawdowns(myReturns)
# table.Stats(myReturns, ci = 0.95, digits = 2) 
# table.SpecificRisk(s$position, b$sma1, Rf = 0, digits = 2) 
# table.Correlation(s$position, b$sma1) 
