#install.packages("caret")
#install.packages("corrplot")
#install.packages("pROC")
#install.packages("FSelector")
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("rJava")
#install.packages("klaR")
#install.packages("MASS")
#install.packages("randomForest")

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

##use set.seed function to ensure the results are repeatable
set.seed(5)

##Read the stock and index data
df_stock = read.csv("C:/Users/Maximilian Merz/Google Drive/Eiffeltower Capital/BaData/BA_EURUSD_15min.txt")

##compute the price change for the stock ans classify as UP/DOWN
price = df_stock$Close-df_stock$Open
class = ifelse(price > 0, "UP","DOWN")

##compute the technical indicators
#Force Index Indicator
forceindex = (df_stock$Close - df_stock$Open)*df_stock$Vol ; forceindex = c(NA,head(forceindex,-1)) ;

#Buy & Sell signal indicators (williams r% and RSI)
willR5 = wpr(df_stock[,c("High","Low",)]);
