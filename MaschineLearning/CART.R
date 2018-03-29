# https://www.r-bloggers.com/forecasting-markets-using-extreme-gradient-boosting-xgboost/
# https://www.quantinsti.com/blog/machine-learning-application-forex-markets-working-models/
library(rpart)
library(rpart.plot)

modelRpart <- rpart(formula = class~., data = train, method = "class",
               control = rpart.control(cp = 0.01))
rpart.plot(modelRpart)
summary(modelRpart)

prediction <- predict(object = model, test)


library(xgboost)
library(car)
xgbData <- data.frame(train)
xgbData = matrix(c(class,willR10,RSI5,ROC5), nrow=length(class))
xgbData = na.omit(xgbData)
colnames(xgbData) = c("class","willR10","RSI5","ROC5")

# Split data into train and test sets 
train_size = 2/3
breakpoint = nrow(xgbData) * train_size

training_data = xgbData[1:breakpoint,]
test_data = xgbData[(breakpoint+1):nrow(xgbData),]

# Split data training and test data into X and Y
X_train = training_data[,2:4] ; Y_train = training_data[,1]
class(X_train)[1]; class(Y_train)

X_test = test_data[,2:4] ; Y_test = test_data[,1]
class(X_test)[1]; class(Y_test)

# Train the xgboost model using the "xgboost" function
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
xgb
xgModel = xgboost(data = dtrain, nround = 5, objective = "binary:logistic")
xgbDataXgb <- xgboost(data = train[,-1], label = label)
