library(party)
library(caret)
data <- dfTest
data_2 <- dfValidation
t <- ctree(
  Endpoint ~ Age + Education2003Revision + Race + MaritalStatus + Sex, 
  data,
  controls = ctree_control(
    teststat="quad",
    testtype="Univariate",
    mincriterion=.95,
    minsplit=10, 
    minbucket=5,
    maxdepth=0
  )
)

plot(t)
### Rplot08.jpeg

# Predict Output
predictions_3 <- predict(t, data)
predictions_3 <- as.data.frame(predictions_3)
mse_3 <- mean((data$Endpoint - predictions_3)^2)
print(mse_3)
### [1] 0.0003321066

# Validate Output
predictions_4 <- predict(t,data_2)
predictions_4 <- as.data.frame(predictions_4)
mse_4 <- mean((data_2$Endpoint - predictions_4)^2)
print(mse_4)
### [1] 0.0003299087

# Confusion Matrix to determine if overfit
predictions_4_1 <- predict(t, data_2)
predictions_4_1 <- as.data.frame(predictions_4_1)
confusionMatrix(data_2$Endpoint, predictions_4_1)

# Input data for determining AUC and ROC Curve
prob_2 <- predict(t, newdata=data, type="response")
pred_2 <- prediction(prob_2, data$Endpoint)
perf_2 <- performance(pred_2, measure = "tpr", x.measure = "fpr")

# Calculations to determine AUC and Plot ROC Curve
auc_2 <- performance(pred_2, measure = "auc")
auc_2 <- auc_2@y.values[[1]]

roc.data_2 <- data.frame(fpr=unlist(perf_2@x.values),
                       tpr=unlist(perf_2@y.values),
                       model="ctree")
ggplot(roc.data_2, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc_2))
### Rplot09.jpeg

print(auc_2)
### [1] 0.8948245