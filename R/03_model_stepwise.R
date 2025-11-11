#' Train Stepwise Model
#' @export
train_stepwise <- function(train_data) {

  library(caret)

  # Set up CV
  cv_control <- trainControl(method = "cv", number = 10)

  # Train model
  model <- train(
    Exam_Score ~ .,
    data = train_data,
    method = "lmStepAIC",
    direction = "backward",
    trControl = cv_control,
    trace = FALSE
  )

  cv_rmse <- model$results$RMSE
  cat("Stepwise CV-RMSE:", round(cv_rmse, 4), "\n")

  return(list(model = model, cv_rmse = cv_rmse))
}
