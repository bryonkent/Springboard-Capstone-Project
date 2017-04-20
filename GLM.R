library(ROCR)
data <- dfTest
data_2 <- dfValidation

new_regression_b1 <- glm(
  Endpoint ~ Age + Education2003Revision + Race + MaritalStatus + Sex, family = binomial())
  new_regression_b1
  summary(new_regression_b1)
  
# Predict Output
predictions_1 <- predict(new_regression_b1, newdata = dfTest, type = "response")
mse_1 <- mean((dfTest$Endpoint - predictions_1)^2)
print(mse_1)
### [1] 0.0003356113

# Validate Output
predictions_2 <- predict(new_regression_b1, newdata = dfValidation, type = "response")
mse_2 <- mean((dfValidation$Endpoint - predictions_2)^2)
print(mse_2)
### [1] 0.0003323429

# Input data for determining AUC and ROC Curve
prob_1 <- predict(new_regression_b1, newdata=dfTest, type="response")
pred_1 <- prediction(prob_1, dfTest$Endpoint)
perf_1 <- performance(pred_1, measure = "tpr", x.measure = "fpr")

# Calculations to determine AUC and Plot ROC Curve
auc_1 <- performance(pred_1, measure = "auc")
auc_1 <- auc_1@y.values[[1]]

roc.data_1 <- data.frame(fpr=unlist(perf_1@x.values),
                       tpr=unlist(perf_1@y.values),
                       model="GLM")
ggplot(roc.data_1, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc_1))
### Rplot07.jpeg

print(auc_1)
### [1] 0.8414791