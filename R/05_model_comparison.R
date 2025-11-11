#' Compare Models
#' @export
compare_models <- function(stepwise_res, lasso_res, ridge_res) {

  # Create comparison table
  results <- data.frame(
    Model = c("Stepwise", "Lasso", "Ridge"),
    CV_RMSE = c(stepwise_res$cv_rmse, lasso_res$cv_rmse, ridge_res$cv_rmse)
  )

  # Sort by RMSE
  results <- results[order(results$CV_RMSE), ]

  print(results)

  # Best model
  best <- results$Model[1]
  cat("\nBest Model:", best, "\n")

  return(results)
}
