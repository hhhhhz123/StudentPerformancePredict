#' Load and Clean Data
#' @export
load_clean_data <- function(file_path) {

  # Load data with factors
  data <- read.csv(file_path, stringsAsFactors = TRUE)

  cat("Original data:", nrow(data), "rows\n")

  # Convert empty strings to NA
  data[data == ""] <- NA

  # Check missing values
  missing_counts <- colSums(is.na(data))
  if (any(missing_counts > 0)) {
    cat("\nMissing values found:\n")
    print(missing_counts[missing_counts > 0])
  }

  # Remove rows with any missing values
  data <- na.omit(data)

  cat("After removing missing values:", nrow(data), "rows\n")

  # Now convert character columns to factors (after removing NAs)
  data <- read.csv(file_path, stringsAsFactors = TRUE)
  data[data == ""] <- NA
  data <- na.omit(data)

  # Rebuild factors without empty levels
  factor_cols <- sapply(data, is.factor)
  for (col in names(data)[factor_cols]) {
    data[[col]] <- factor(data[[col]])
  }

  return(data)
}
