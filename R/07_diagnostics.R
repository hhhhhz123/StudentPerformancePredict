#' Simple Model Evaluation
#' @export
evaluate_model <- function(model, test_data) {

  # Load training data to get factor levels
  load("../data/processed/train_data.rda")

  # Fix factor levels in test data
  for (col in names(train_data)) {
    if (is.factor(train_data[[col]]) && col != "Exam_Score") {
      test_data[[col]] <- factor(test_data[[col]],
                                 levels = levels(train_data[[col]]))
    }
  }

  # Make predictions
  predictions <- predict(model, newdata = test_data)
  actual <- test_data$Exam_Score
  residuals <- actual - predictions

  # Calculate metrics
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  r2 <- cor(actual, predictions)^2

  # Print results
  cat("RMSE: ", round(rmse, 4), "\n")
  cat("MAE:  ", round(mae, 4), "\n")
  cat("R²:   ", round(r2, 4), "\n")

  return(list(
    predictions = predictions,
    actual = actual,
    residuals = residuals,
    rmse = rmse,
    mae = mae,
    r2 = r2
  ))
}


#' Simple Diagnostic Plots
#' @export
plot_diagnostics <- function(eval_result) {

  library(ggplot2)

  par(mfrow = c(2, 3))

  # 1. Predicted vs Actual
  plot(eval_result$actual, eval_result$predictions,
       xlab = "Actual", ylab = "Predicted",
       main = "Predicted vs Actual")
  abline(0, 1, col = "red", lwd = 2)

  # 2. Residuals vs Fitted
  plot(eval_result$predictions, eval_result$residuals,
       xlab = "Fitted", ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red", lwd = 2)

  # 3. Q-Q Plot
  qqnorm(eval_result$residuals)
  qqline(eval_result$residuals, col = "red", lwd = 2)

  # 4. Histogram of Residuals
  hist(eval_result$residuals, breaks = 30,
       main = "Histogram of Residuals",
       xlab = "Residuals", col = "lightblue")

  # 5. Scale-Location
  plot(eval_result$predictions, sqrt(abs(eval_result$residuals)),
       xlab = "Fitted", ylab = "Sqrt(|Residuals|)",
       main = "Scale-Location")

  # 6. Residuals vs Order
  plot(1:length(eval_result$residuals), eval_result$residuals,
       xlab = "Order", ylab = "Residuals",
       main = "Residuals vs Order")
  abline(h = 0, col = "red", lwd = 2)

  par(mfrow = c(1, 1))
}


#' Simple Statistical Tests
#' @export
residual_tests <- function(eval_result) {

  residuals <- eval_result$residuals
  predictions <- eval_result$predictions

  # 1. Shapiro-Wilk Test
  shapiro_test <- shapiro.test(residuals)
  cat("Shapiro-Wilk Test (Normality):\n")
  cat("  p-value:", round(shapiro_test$p.value, 4), "\n")
  cat("  Result:", ifelse(shapiro_test$p.value > 0.05, "Normal", "Not Normal"), "\n\n")

  # 2. Breusch-Pagan Test
  bp_model <- lm(residuals^2 ~ predictions)
  bp_pvalue <- summary(bp_model)$coefficients[2, 4]
  cat("Breusch-Pagan Test (Homoscedasticity):\n")
  cat("  p-value:", round(bp_pvalue, 4), "\n")
  cat("  Result:", ifelse(bp_pvalue > 0.05, "Homoscedastic", "Heteroscedastic"), "\n\n")

  # 3. Durbin-Watson
  dw_stat <- sum(diff(residuals)^2) / sum(residuals^2)
  cat("Durbin-Watson Test (Independence):\n")
  cat("  Statistic:", round(dw_stat, 4), "\n")
  cat("  Result:", ifelse(abs(dw_stat - 2) < 0.5, "Independent", "Correlated"), "\n")
}


#' Simple Error Analysis
#' @export
error_analysis <- function(eval_result) {

  residuals <- eval_result$residuals

  cat("Error Statistics:\n")
  cat("  Mean:   ", round(mean(residuals), 4), "\n")
  cat("  Median: ", round(median(residuals), 4), "\n")
  cat("  SD:     ", round(sd(residuals), 4), "\n\n")

  cat("Prediction Accuracy:\n")
  cat("  Within ±5:  ", round(mean(abs(residuals) <= 5) * 100, 1), "%\n")
  cat("  Within ±10: ", round(mean(abs(residuals) <= 10) * 100, 1), "%\n")
}
