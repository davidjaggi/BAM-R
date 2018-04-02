##### Install Packages #########################################################
# install.packages("caret")
# install.packages("corrplot")
# install.packages("pROC")
# install.packages("FSelector")
# install.packages("lattice")
# install.packages("ggplot2")
# install.packages("rJava")
# install.packages("klaR")
# install.packages("MASS")
# install.packages("randomForest")
# install.packages("randomForestExplainer")
# install.packages("mlbench")
# install.packages("lubridate")
# install.packages("xts")
# install.packages("FGN")
# install.packages("candlesticks", repos="http://R-Forge.R-project.org")
# install_github("IlyaKipnis/DSTrading")
# install.packages("dplyr")
# install.packages("tdyr")
# install.packages("data.table")

##### Install Libraries ########################################################
library(quantmod)
library(TTR)
library(lattice)
library(ggplot2)
library(caret)
library(corrplot)
library(pROC)
library(FSelector)
library(rJava)
library(klaR)
library(MASS)
library(randomForest)
library(kernlab)
library(rpart)
library(randomForestExplainer)
library(mlbench)
library(lubridate)
library(xts)
library(FGN)
library(candlesticks)
library(dplyr)
library(tidyr)
library(data.table)
source("MaschineLearning/Functions.R")
##### Input your settings here #################################################
# use set.seed function to ensure the results are repeatable
set.seed(5)

# Set the date for the timeseries
start = "2012-01-01"
end = as.character(today())
path = "Data/BA_EURUSD_60min.txt"
# path = "Data/BA_EURUSD_15min.txt"

data <- DataLoader(path, start, end)
##compute the price change for the stock ans classify as UP/DOWN
price = as.vector(data$Close-data$Open)
class = as.vector(ifelse(price > 0, "UP","DOWN"))
return = Delt(data$Close, k=1)
return2 = SumReturns(data = return, n = 2)
return4 = SumReturns(data = return, n = 4)
return8 = SumReturns(data = return, n = 8)
return12 = SumReturns(data = return, n = 12)
return16 = SumReturns(data = return, n = 16)
return20 = SumReturns(data = return, n = 20)
return24 = SumReturns(data = return, n = 24)

##### Import all Indicators  ###################################################
source("MaschineLearning/Indicators.R")
##### Combining all indicators and classes into one dataframe ##################

# dataset = data.frame(class,forceindex,willR2,willR5,willR10,willR15,RSI2,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10,ATR2,ATR5,ATR10,HC,CL,AroonH, AroonD)
dataset <- data.frame(class, MOM5, MOM10)
dataset <- SignalLagger(Dataset = dataset, Lags = 1)

##understanding the dataset using descriptive statistics
head(dataset)
print(head(dataset),5)
dim(dataset)
y = dataset$class
cbind(freq=table(y), percentage=prop.table(table(y))*100)

summary(dataset)

##visualising the dataset using a correlation matrix
correlations = cor(dataset[,c(2:ncol(dataset))])
print(head(correlations))
corrplot(correlations, method="number")

# Have a look at the pairs
pairs(dataset[1:500,])

##### Split data into train and test ###########################################
train <- train_sample(dataset, 0.75)
test <- test_sample(dataset, 0.75)
