#' Split Data into Train and Test
#' @export
split_data <- function(data, train_prop = 0.7) {

  set.seed(123)

  n <- nrow(data)
  train_index <- sample(1:n, train_prop * n)

  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]

  cat("Training:", nrow(train_data), "rows\n")
  cat("Test:", nrow(test_data), "rows\n")

  return(list(train = train_data, test = test_data))
}
