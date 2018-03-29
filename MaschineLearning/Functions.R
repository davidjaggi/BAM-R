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