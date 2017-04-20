library(randomForest)

# Fitting model
fit_RF <- randomForest(
  Endpoint ~ Age + Education2003Revision + Race + MaritalStatus, 
  dfTest,
  ntree=1000
)
summary(fit_RF)

#Predict Output 
predicted_RF <- predict(fit_RF,dfValidation)
summary(predicted_RF)