DataLoader <- function(path, start, end){
  data <- read.csv(path)
  data$Datetime <- strptime(paste(data$Date, data$Time), "%m/%d/%Y %H:%M")
  data <- as.xts(x = data[,3:7], order.by = data$Datetime)
  data <- data[paste0(start,"::",end)]
  return(data)
}

SignalLagger <- function(Dataset, Lags = 1, Normal = TRUE){
  Dataset <- Dataset
  Signal <- Dataset[,1]
  Lagged <- Dataset[,(2:ncol(Dataset))]
  if (length(Lags) != 1){
    Lagged <- lead()
  } else{
  Lagged <- as.data.frame(c(NA, head(,-Lags)))
  }
  LaggedData <- as.data.frame(cbind(Signal, Lagged))
  LaggedData <- na.omit(LaggedData)
  return(LaggedData)
}

train_sample <- function(data, percentage){
  smp_size <- floor(percentage * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  train <- data[train_ind, ]
  return(data)
}

test_sample <- function(data, percentage){
  smp_size <- floor(percentage * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  test <- data[-train_ind, ]
  return(test)
}

PercentageBody <- function(Open,High,Low,Close){
  divisor <- abs(Close-Open)
  dividend <- High-Low
  PercentageBody <- ifelse(dividend > 0, divisor/dividend, 1)
  return(PercentageBody)
}

UpperWick <- function(Open,High,Low,Close){
  upper <- ifelse(Open >= Close, Open, Close)
  UpperWick <- High-upper
  return(UpperWick)
}

DownWick <- function(Open,High,Low,Close){
  lower <- ifelse(Open <= Close, Open, Close)
  DownWick <- lower-Low
  return(DownWick)
}

RelativeClose <- function(Open,High,Low,Close){
  divisor <- Close-Low
  dividend <- High-Low
  RelativeClose <- ifelse(dividend > 0, divisor/dividend, 1)
  return(RelativeClose)
}

SumReturns <- function(data, n){
  data <- data
  n <- n
  dataShift <- lag(x = data, k = -(n-1))
  sum <- rollsumr(dataShift, k = n, fill = NA)
  return(sum)
}
