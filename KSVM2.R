library(e1071)
data <- dfTest
data_2 <- dfValidation
m <- svm(Endpoint ~ Age + Education2003Revision + 
           Race + MaritalStatus + Sex, data)

# Predict Output 
predictions_5 <- predict(m, data)
mse_5 <- mean((data$Endpoint - prediction_5)^2)
print(mse_5)

# Validate Output
predictions_6 <- predict(m, data_2)
mse_6 <- mean((data_2$Endpoint - prediction_6)^2)
print(mse_6)

# Input data for determining AUC and ROC Curve
prob_3 <- predict(m, newdata=data, type="response")
pred_3 <- prediction(prob_3, data$Endpoint)
perf_3 <- performance(pred_3, measure = "tpr", x.measure = "fpr")

# Calculations to determine AUC and Plot ROC Curve
auc_3 <- performance(pred_3, measure = "auc")
auc_3 <- auc_3@y.values[[1]]

roc.data_3 <- data.frame(fpr=unlist(perf_3@x.values),
                         tpr=unlist(perf_3@y.values),
                         model="svm")
ggplot(roc.data_3, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc_3))