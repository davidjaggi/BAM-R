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
# install.packages(xts)

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



# use set.seed function to ensure the results are repeatable
set.seed(5)

##Read the stock and index data
data = read.csv("Data/BA_EURUSD_15min.txt")

data$Datetime <- strptime(paste(data$Date, data$Time), "%m/%d/%Y %H:%M")
data <- as.xts(x = data[,3:7], order.by = data$Datetime)
##compute the price change for the stock ans classify as UP/DOWN
price = data$Close-data$Open
class = ifelse(price > 0, "UP","DOWN")

##### Inport all Indicators  ###################################################
source("MaschineLearning/Indicators.R")

##### Combining all indicators and classes into one dataframe ##################
dataset = data.frame(class,forceindex,willR2,willR5,willR10,willR15,RSI2,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10,ATR2,ATR5,ATR10,HC,CL,AroonH, AroonD)
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

# Have a look at the pairs
pairs(dataset[1:500,])

##### Split data into train and test ###########################################
# 75% of the sample size
# Create a train and test Dataset


smp_size <- floor(0.75 * nrow(dataset))
  
## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]

