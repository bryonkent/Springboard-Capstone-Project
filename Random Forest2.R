library(randomForest)

# create a subset of the larger data.frame to work with in R
df_sample_2 <- sample(nrow(df), size = 10000, replace = TRUE)
df_subset_2 <- df[df_sample,]

# Fitting model
fit_RF <- randomForest(
  Endpoint ~ Age + Education2003Revision + Race + MaritalStatus + Sex, 
  data = df_subset_2
)
summary(fit_RF)

#Predict Output 
predictions_7 <- predict(fit_RF,df_subset_2)
mse_7 <- mean((df_subset_2$Endpoint - predictions_7)^2)
print(mse_7)
### mse_7 = 0.0007220855

#Validate Output
predictions_8 <- predict(fit_RF, df)
mse_8 <- mean((df$Endpoint - predictions_8)^2)
print(mse_8)
### mse_8 = 0.0003319365

# Input data for determining AUC and ROC Curve
prob_4 <- predict(fit_RF, newdata=df_subset_2, type="response")
pred_4 <- prediction(prob_4, df_subset_2$Endpoint)
perf_4 <- performance(pred_4, measure = "tpr", x.measure = "fpr")

# Calculations to determine AUC and Plot ROC Curve
auc_4 <- performance(pred_4, measure = "auc")
auc_4 <- auc_4@y.values[[1]]

roc.data_4 <- data.frame(fpr=unlist(perf_4@x.values),
                         tpr=unlist(perf_4@y.values),
                         model="randomForest")
ggplot(roc.data_4, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc_4))
### Rplot11.jpeg

print(auc_4)
### auc_4 = 0.993157