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


##use set.seed function to ensure the results are repeatable

set.seed(5)

##Read the stock and index data
data = read.csv("Data/BA_EURUSD_15min.txt")

##compute the price change for the stock ans classify as UP/DOWN
price = data$Close-data$Open
class = ifelse(price > 0, "UP","DOWN")

##compute the technical indicators
#Force Index Indicator
forceindex = (data$Close - data$Open)*data$Vol ; forceindex = c(NA,head(forceindex,-1)) ;

#Buy & Sell signal indicators (williams r% and RSI)
willR5 = WPR(data[,c("High","Low","Close")], n=5) ; willR5 = c(NA,head(willR5,-1));
willR10 = WPR(data[,c("High","Low","Close")], n=10) ; willR5 = c(NA,head(willR10,-1));
willR15 = WPR(data[,c("High","Low","Close")], n=15) ; willR5 = c(NA,head(willR15,-1));

RSI2 = RSI(data$Close, n = 2, matype = "WMA"); RSI2 = c(NA, head(RSI2m-1));
RSI5 = RSI(data$Close, n = 5, matype="WMA"); RSI5 = c(NA,head(RSI5,-1));
RSI10 = RSI(data$Close, n = 10, matype="WMA"); RSI10 = c(NA,head(RSI10,-1));
RSI15 = RSI(data$Close, n = 15, matype="WMA"); RSI15 = c(NA,head(RSI15,-1)); 

#Price xhange indicators
ROC5 = ROC(data$Close, n = 5, type = "discrete")*100 ; ROC5 = c(NA,head(ROC5,-1)) ;
ROC10 = ROC(data$Close, n = 10, type = "discrete")*100 ; ROC10 = c(NA,head(ROC10, -1)) ;

MOM5 = momentum(data$Close, n = 5, na.pad = TRUE) ; MOM5 = c(NA,head(MOM5, -1)) ;
MOM10 = momentum(data$Close, n = 10, na.pad = TRUE) ; MOM10 = c(NA,head(MOM10, -1)) ;

#MomIndexes to be inserted

#ATR
ATR2 = ATR(data[,c("High","Low","Close")], n=2, matype="WMA")[,1] ; ATR2 = c(NA,head(ATR2,-1)) ;
ATR5 = ATR(data[,c("High","Low","Close")], n=5, matype="WMA")[,1] ; ATR5 = c(NA,head(ATR5,-1)) ;
ATR10 = ATR(data[,c("High","Low","Close")], n=10, matype="WMA")[,1] ; ATR10 = c(NA,head(ATR10,-1)) ;

#ATRIndexes to be inserted

# Difference between High and Close
HC = data$High - data$Close; HC = c(NA, head(HC,-1));

# Difference between Low and Close
CL = data$Close - data$Low; CL = c(NA, head(CL, -1));

# Add Aroon indicator
Aroon = aroon(data$High, n = 5); AroonH = c(NA, head(Aroon[,3],-1));
Aroon = aroon(data$Low, n = 5); AroonD = c(NA, head(Aroon[,3], -1));

##Combining all indicators and classes into one dataframe
dataset = data.frame(class,forceindex,willR5,willR10,willR15,RSI2,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10,ATR2,ATR5,ATR10,HC,CL,AroonH, AroonD)
dataset = na.omit(dataset)

##understanding the dataset using descriptive statistics
print(head(dataset),5)
dim(dataset)
y = dataset$class
cbind(freq=table(y), percentage=prop.table(table(y))*100)

summary(dataset)

##visualising the dataset using a correlation matrix
correlations = cor(dataset[,c(2:ncol(dataset))])
print(head(correlations))
corrplot(correlations, method="number")

##using randomForest
fit_rf = randomForest(as.factor(class)~., data = dataset)

importance(fit_rf)

varImpPlot(fit_rf)
getTree(fit_rf)

# Plot the randomForest error
plot(fit_rf)


# Resampling method used - 10fold cross validatation
# with accuracy as the model evaluation metric
trainControl = trainControl (method="cv", number=10)
metric = "Accuracy"
