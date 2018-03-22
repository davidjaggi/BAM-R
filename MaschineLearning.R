library(quantmod)
library(TTR)
# Load data
data <- read.csv('C:/Users/David Jaggi/Google Drive/Eiffeltower Capital/BaData//BA_EURUSD_15min.txt')

# Calculate returns
data$Change <- Delt(data$Close)
data$Move <- ifelse(data$Change > 0, 1,-1)

data$VolChange <- Delt(data$Vol)

data$Atr <- ATR(HLC = HLC(data), n = 20)
data$Atr <- ifelse()

data$AtrChange <- ifelse(Lag(data$Atr,1) > 0, Delt(data$Atr), 0)

data <- na.omit(data)

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
