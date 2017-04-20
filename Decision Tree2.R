library(rpart)

#grow tree
fit <- rpart(
  Endpoint ~ Age + Education2003Revision + Race + MaritalStatus,
  dfTest,
  method = "class"
)
summary(fit)

#predict output
predicted <- predict(fit,dfValidation)
summary(predicted)