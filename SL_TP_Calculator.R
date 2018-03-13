library(xts)
library(readxl)
library(TTR)
library(quantmod)
library(dplyr)
source("BAM_Functions.R")

OHLC <- read.csv("C:/Users/David Jaggi/Google Drive/Eiffeltower Capital/BaData/BA_EURUSD_15min.txt", header=FALSE)
colnames(OHLC) <- c("Date","Time","Open","High","Low","Close","Vol","OI")
OHLC <- OHLC[-1,]
summary(OHLC)

OHLC$Datetime <- strptime(paste(OHLC$Date, OHLC$Time), "%m/%d/%Y %H:%M")

data <- as.xts(x = OHLC, order.by = OHLC$Datetime)
data <- data[,c(4,5,6)]
data$ATR <- BAM_ATR(HLC = HLC(data), n = 20)
data$CLSD <- rollapplyr((data$Close- data$Low), 20, sd, fill = 0)
data <- na.omit(data)
data <- data["2016-02-12/"]

trades <- read_excel("C:/Users/David Jaggi/Google Drive/Eiffeltower Capital/MCBacktestReports/EURUSD.FXCM  Test Back-Testing Strategy Performance Report.xlsx", 
                     sheet = "List of Trades", skip = 1, col_types = c(rep("numeric", 2), rep("text",2), rep("guess",2), rep("numeric",10)))
trades$Time <- strftime(trades$Time, format = "%H:%M")
trades$Date <- format(as.Date(trades$Date, '%Y-%m-%d'), "%m/%d/%Y")
trades$Datetime <- strptime(paste(trades$Date, trades$Time), "%m/%d/%Y %H:%M")
trades <- as.xts(x = trades[,-c(4:6,17)], order.by = trades$Datetime)
trades$Type[trades$Type == "EntryLong"] <- 1
trades$Type[trades$Type == "ExitLong"] <- -1
colnames(trades) <- c("Trade", "Order","Type","Price","Contracts","Profit","Profit_pct","Cum_Profit","Cum_Profit_pct","Runup","Runup_pct","Drawdown","Drawdown_pct")
storage.mode(trades) <- "numeric"
trades$Trade <- na.locf(as.numeric(trades$Trade))
trades[is.na(trades)] <- 0

table(summary(trades))

entries <- trades[seq(1,nrow(trades),2),]
entries <- as.xts(x = entries, order.by = entries$Datetime)
entries <- entries[,c(4,7)]
entries$Entry <- 1

exits <- trades[seq(2,nrow(trades),2),]
exits <- as.xts(x = exits, order.by = exits$Datetime)
exits <- exits[,c(4,7)]

df <- data
signals <- merge(entries, exits)

df <- merge(data, entries[,c(3,2)])
df <- na.fill(object = df, fill = 0)

df$CL1 <- (df$Close - lag.xts(df$Low,-1))/df$ATR
df$CL5 <- (df$Close - lag.xts(df$Low,-5))/df$ATR
df$CL10 <- (df$Close - lag.xts(df$Low,-10))/df$ATR
df$CL15 <- (df$Close - lag.xts(df$Low,-15))/df$ATR

df$CLSD1 <- (df$Close - lag.xts(df$Low, -1))/df$CLSD

par(mfrow = c(1,2))
hist(df$CL1[df$Entry == 1], breaks = 20, xlim = c(-15,15))
hist(df$CLSD1[df$Entry == 1], breaks = 20, xlim = c(-15,15))

hist(df$CL5[df$Entry == 1], breaks = 20, xlim = c(-15,15))
hist(df$CL10[df$Entry == 1], breaks = 20, xlim = c(-15,15))
hist(df$CL15[df$Entry == 1], breaks = 20, xlim = c(-15,15))


summary(df$CL1[df$Entry == 1])
summary(df$CL5[df$Entry == 1])
summary(df$CL15[df$Entry == 1])


df$HC1 <- (-df$Close + lag.xts(df$High,-1))/df$ATR
df$HC5 <- (-df$Close + lag.xts(df$High,-5))/df$ATR
df$HC10 <- (-df$Close + lag.xts(df$High,-10))/df$ATR
df$HC15 <- (-df$Close + lag.xts(df$High,-15))/df$ATR


hist(df$HC1[df$Entry == 1], breaks = 20, xlim = c(-15,15))
hist(df$HC5[df$Entry == 1], breaks = 20, xlim = c(-15,15))
hist(df$HC10[df$Entry == 1], breaks = 20, xlim = c(-15,15))
hist(df$HC15[df$Entry == 1], breaks = 20, xlim = c(-15,15))

summary(df$HC1[df$Entry == 1])
summary(df$HC5[df$Entry == 1])
summary(df$HC15[df$Entry == 1])


inds = which(df$Entry == 1)
# We use lapply() to get all rows for all indices, result is a list
rows <- lapply(inds, function(x) (x+15))
# With unlist() you get all relevant rows
df[unlist(rows),]

series <- df[df$Entry != 0]
series <- unlist(series[,c("CL1", "CL5", "CL15")])


