library(e1071)
library(MASS)
library(ROCR)
attach(df)

# create a subset of the larger data.frame to work with in R
df_sample <- sample(nrow(df), size = 10000, replace = TRUE)
df_subset <- df[df_sample,]


# Divide dfTest data to x (contains all the features) and y only the classes
x <- subset(df_subset, select=-Endpoint)
y <- df_subset$Endpoint

# Create the SVM model and show the summary
m <- svm(formula = Endpoint ~ ., data=df_subset)
summary(m)

# Predict Output (dfTest might be x in this case)
predictions_5 <- predict(m, x)

mse_5 <- mean((y - predictions_5)^2)
print(mse_5)
# mse_5 = 0.008890168

# Examine the confusion matrix results using the command table to compare the results of the SVM prediction and the class data in the y variable
table(predictions_5, y)

# Tune the SVM to find the best cost and gamma
svm_tune <- tune(svm, train.x = x, train.y = y,
                 Kernel = "radial", ranges = list(cost = 10^(-1:2), gamma = c(.5, 1, 2)))
print(svm_tune)

# After you find the best cost and gamma, you can create svm model again and try to run again. The * is a place holder for the cost and gamma shown in the printed svm_tune
svm_model_after_tune <- svm(Endpoint~ ., data = df_subset, kernel = "radial", cost = 1, gamma = 0.03703704)
summary(svm_model_after_tune)

# Run predictions again with the new model
Predictions_5_1 <- predict(svm_model_after_tune, x)

# Examine the confusion matrix result of prediction, using command table to compare the result of SVM prediction and the class data in y variable.
table(Predictions_5_1, y)

# Validate Output
predictions_6 <- predict(svm_model_after_tune, df)
mse_6 <- mean((df$Endpoint - predictions_6)^2)
print(mse_6)
### mse_6 = 0.008517287

# Input data for determining AUC and ROC Curve
prob_3 <- predict(svm_model_after_tune, newdata=df_subset, type="response")
pred_3 <- prediction(prob_3, y)
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
### Rplot010.jpeg

print(auc_3)
### auc_3 = 0.9832366