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
df_stock = read.csv("Data/BA_EURUSD_15min.txt")

##compute the price change for the stock ans classify as UP/DOWN
price = df_stock$Close-df_stock$Open
class = ifelse(price > 0, "UP","DOWN")

##compute the technical indicators
#Force Index Indicator
forceindex = (df_stock$Close - df_stock$Open)*df_stock$Vol ; forceindex = c(NA,head(forceindex,-1)) ;

#Buy & Sell signal indicators (williams r% and RSI)
willR5 = WPR(df_stock[,c("High","Low","Close")], n=5) ; willR5 = c(NA,head(willR5,-1));
willR10 = WPR(df_stock[,c("High","Low","Close")], n=10) ; willR5 = c(NA,head(willR10,-1));
willR15 = WPR(df_stock[,c("High","Low","Close")], n=15) ; willR5 = c(NA,head(willR15,-1));

RSI5 = RSI(df_stock$Close, n = 5, matype="WMA"); RSI5 = c(NA,head(RSI5,-1));
RSI10 = RSI(df_stock$Close, n = 10, matype="WMA"); RSI10 = c(NA,head(RSI10,-1));
RSI15 = RSI(df_stock$Close, n = 15, matype="WMA"); RSI15 = c(NA,head(RSI15,-1)); 

#Price xhange indicators
ROC5 = ROC(df_stock$Close, n = 5, type = "discrete")*100 ; ROC5 = c(NA,head(ROC5,-1)) ;
ROC10 = ROC(df_stock$Close, n = 10, type = "discrete")*100 ; ROC10 = c(NA,head(ROC10, -1)) ;

MOM5 = momentum(df_stock$Close, n = 5, na.pad = TRUE) ; MOM5 = c(NA,head(MOM5, -1)) ;
MOM10 = momentum(df_stock$Close, n = 10, na.pad = TRUE) ; MOM10 = c(NA,head(MOM10, -1)) ;

#MomIndexes to be inserted

#ATR
ATR5 = ATR(df_stock[,c("High","Low","Close")], n=5, matype="WMA")[,1] ; ATR5 = c(NA,head(ATR5,-1)) ;
ATR10 = ATR(df_stock[,c("High","Low","Close")], n=10, matype="WMA")[,1] ; ATR10 = c(NA,head(ATR10,-1)) ;

#ATRIndexes to be inserted

##Combining all indicators and classes into one dataframe
dataset = data.frame(class,forceindex,willR5,willR10,willR15,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10,ATR5,ATR10)
dataset = na.omit(dataset)

##understanding the dataset using descriptive statistics
print(head(dataset),5)
dim(dataset)
y = dataset$class
cbind(freq=table(y), percentage=prop.table(table(y))*100)

summary(dataset)

##visualising the dataset using a correlation matrix
correlations = cor(dataset[,c(2:14)])
print(head(correlations))
corrplot(correlations, method="number")

##using randomForest
fit_rf = randomForest(as.factor(class)~., data = dataset)
importance(fit_rf)

varImpPlot(fit_rf)
getTree(fit_rf)




# Resampling method used - 10fold cross validatation
# with accuracy as the model evaluation metric
trainControl = trainControl (method="cv", number=10)
metric = "Accuracy"
