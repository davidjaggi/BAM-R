library(rpart)
library(rpart.plot)

model <- rpart(formula = class~., data = train, method = "class")
rpart.plot(model)

prediction <- predict(object = model, )