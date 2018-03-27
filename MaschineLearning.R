library(quantmod)
library(TTR)
library(xts)
# Load data
data <- read.csv('C:/Users/David Jaggi/Google Drive/Eiffeltower Capital/BaData//BA_EURUSD_15min.txt', 
                 header = TRUE, sep = ',')

data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format="%m/%d/%Y %H:%M")
data <- as.xts(x = data[,-c(1:2,8:9)], order.by = data$Datetime)


# Calculate returns
data$Change <- Delt(data$Close)
data$Move <- ifelse(data$Change > 0, 1,-1)

data$VolChange <- Delt(data$Vol)

data$Atr <- ATR(HLC = HLC(data), n = 20)
data$AtrChange <- ifelse(data$Atr - Lag(data$Atr,1) > 0, 1, 0)

data$AtrChange <- ifelse(Lag(data$Atr,1) > 0, Delt(data$Atr), 0)

data <- na.omit(data)

data <- data["2017-01-01/"]

data <- data[,c(1:8,12:13)]
# Choose indicators Volumechange

# Introduce lag 
data$Move <- c(data$Move[-1], NA)

table(data$Move)
# Load randomForest package
library(randomForest)

plot(data$Close, type = 'l')

set.seed(100)
trainSplit <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
train <- data[trainSplit,]
test <- data[-trainSplit,]
summary(train)
summary(test)


str(data)
model <- randomForest(Move~Open+High+Low+Close+Vol,train)
