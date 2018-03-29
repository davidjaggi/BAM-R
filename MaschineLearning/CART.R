library(rpart)
library(rpart.plot)

model <- rpart(formula = class~., data = train, method = "class",
               control = rpart.control(cp = 0.05))
rpart.plot(model)
text(model)

prediction <- predict(object = model, test)
