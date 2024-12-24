#+++++++++++++++++++
### ELASTIC NET ####
#+++++++++++++++++++

library(glmnet)
library(pROC)

# Vector of alpha values to experiment with
alpha_values <- c(0, 0.2, 0.5, 0.9, 1)

# Create empty vectors to store results
accuracy_values <- numeric(length(alpha_values))
precision_values <- numeric(length(alpha_values))
recall_values <- numeric(length(alpha_values))
f1_values <- numeric(length(alpha_values))
auc_values <- numeric(length(alpha_values))

coefficients_list <- list()
models_list <- list()

# Iterate over different alpha values
for (i in seq_along(alpha_values)) {
  set.seed(2023)
  a <- alpha_values[i]
  
  # Train the model
  model <- cv.glmnet(model.matrix(Success ~ . - 1, data = train), train$Success, family = "binomial", alpha = a, nfolds = 20)
  
  models_list[[paste0("model_alpha_", a)]] <- model
  
  # Make predictions on the test set
  test_model_matrix <- model.matrix(Success ~ . - 1, data = test)
  predictions <- predict(model, newx = test_model_matrix, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  roc_curve <- roc(test$Success, predictions)
  auc_value <- auc(roc_curve)
  
  plot(roc_curve ,main ="ROC curve")
  
  auc_values[i] <- auc_value
  
  # Calculate performance metrics
  conf_matrix <- table(predicted_labels, test$Success)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Store the results
  accuracy_values[i] <- accuracy
  precision_values[i] <- precision
  recall_values[i] <- recall
  f1_values[i] <- f1_score
  
  coef.est <- as.matrix(coef(model, s=model$lambda.min))
  coef.vec <- subset(coef.est, coef.est != 0)  
  
  coefficients_list[[paste0("coefficients_alpha_", a)]] <- subset(coef.est, coef.est != 0)
  
  result_df <- data.frame(Alpha = alpha_values, Accuracy = accuracy_values, Precision = precision_values, Recall = recall_values, F1 = f1_values, AUC = auc_values)
}

# Display results
print(result_df)

ggplot(result_df, aes(x = Alpha, y = AUC)) +
  geom_line() +
  labs(title = "AUC Curve for Different Alpha Values",
       x = "Alpha",
       y = "AUC") +
  theme_minimal()

model <- glmnet(model.matrix(Success ~ . - 1, data = train), train$Success, family = "binomial", alpha = 0.5)

# Display coefficients 
View(coefficients_list)

#Display models
View(models_list)

plot(models_list[[3]], xvar = "lambda", label = TRUE)
plot(models_list[[3]], xvar = "lambda", label = TRUE, xlim = c(-3.5, -1.5))  # Adjust the range as needed


####

#### RANDOM FOREST ####
library(randomForest)

# Specify the response as a factor with two levels in the randomForest function
rf_model <- randomForest(Rating_Average ~ ., data = train, ntree = 200, importance=TRUE)
print(rf_model)

mtry <- tuneRF(movies[-3],movies$Rating_Average, ntreeTry=200,
               stepFactor=2,improve=0.05, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

rf_model <- randomForest(Rating_Average ~ ., data = train, ntree = 200, importance=TRUE, mtry=best.m)
print(rf_model)

predictions <- predict(rf_model, newdata = test)

predictions_numeric <- as.numeric(levels(predictions))[predictions]
predicted_labels <- ifelse(predictions_numeric > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- table(predicted_labels, test$Rating_Average)
# Calculate accuracy, precision, recall
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

cat("Classification Results:\n","Accuracy:", round(accuracy, 3), "\n", "Precision:", round(precision, 3), "\n", 
    "Recall", round(recall, 3), "\n\n")
conf_matrix
