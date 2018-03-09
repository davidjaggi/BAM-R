BAM_ATR <- function (HLC, n = 14, maType, ...){
  HLC <- try.xts(HLC, error = as.matrix)
  if (is.xts(HLC)) {
    closeLag <- lag.xts(HLC[, 3])
  }
  else {
    closeLag <- c(NA, HLC[-NROW(HLC), 3])
  }
  trueHigh <- pmax(HLC[, 1], closeLag, na.rm = FALSE)
  trueLow <- pmin(HLC[, 2], closeLag, na.rm = FALSE)
  tr <- as.numeric(trueHigh) - as.numeric(trueLow)
  maArgs <- list(n = n, ...)
  if (missing(maType)) {
    maType <- "EMA"
    maArgs$wilder <- TRUE
  }
  atr <- do.call(maType, c(list(tr), maArgs))
  result <- cbind(atr)
  colnames(result) <- c("ATR")
  reclass(result, HLC)
}

