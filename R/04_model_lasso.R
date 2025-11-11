#' Train Lasso Model
#' @export
train_lasso <- function(train_data) {

  library(glmnet)

  # Prepare data
  X <- model.matrix(Exam_Score ~ . - 1, data = train_data)
  y <- train_data$Exam_Score

  # Train model with CV
  model <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

  cv_rmse <- sqrt(min(model$cvm))
  cat("Lasso CV-RMSE:", round(cv_rmse, 4), "\n")

  return(list(model = model, cv_rmse = cv_rmse))
}


#' Train Ridge Model
#' @export
train_ridge <- function(train_data) {

  library(glmnet)

  X <- model.matrix(Exam_Score ~ . - 1, data = train_data)
  y <- train_data$Exam_Score

  model <- cv.glmnet(X, y, alpha = 0, nfolds = 10)

  cv_rmse <- sqrt(min(model$cvm))
  cat("Ridge CV-RMSE:", round(cv_rmse, 4), "\n")

  return(list(model = model, cv_rmse = cv_rmse))
}
