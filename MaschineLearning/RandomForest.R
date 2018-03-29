# using randomForest

tuneRF(x = train[,2:ncol(dataset)], y = train$class)
# Best mtry was 16

fit_rf = randomForest(as.factor(class)~., data = train, replace = TRUE, mtry = 1, ntree = 2000, importance = TRUE)
fit_rf

# Select the most important values
importance(fit_rf)

varImpPlot(fit_rf)

# The most important values are WillR, RSI5 and ROC5d
# dataset_crop = data.frame(class,willR5,willR10,willR15,RSI2,RSI5,RSI10,RSI15,ROC5,ROC10)
dataset_crop = data.frame(class,RSI2,PercBody,Hour)
dataset_crop = na.omit(dataset_crop)

train <- train_sample(dataset_crop, 0.75)
test <- test_sample(dataset_crop, 0.75)

# visualising the dataset_crop using a correlation matrix
correlations = cor(dataset_crop[,c(2:ncol(dataset_crop))])
print(head(correlations))
corrplot(correlations, method="number")

dataset_crop = data.frame(class,willR10,RSI5,ROC5)
dataset_crop = na.omit(dataset_crop)


# visualising the dataset_crop using a correlation matrix
correlations = cor(dataset_crop[,c(2:ncol(dataset_crop))])
print(head(correlations))
corrplot(correlations, method="number")

fit_rfcrop <- randomForest(as.factor(class)~., data = train, replace = TRUE, importance = TRUE)
fit_rfcrop

getTree(fit_rfcrop)
importance(fit_rfcrop)
varImpPlot(fit_rfcrop)
