
#' Separate Training and Test data
#'
#' @param data_name Name of the data.
#' @param data_classes List of data class for all columns.
#' @param output_var The name of the column containing the output.
#' @param drop_columns Which columns should be removed from the data.
#' @param train_test_ratio Ratio of training data of the whole data.
#'
#' @return The training-test split.
#' @export
create_data_splitDS <- function(data_name, data_classes, output_var,
                                drop_columns, train_test_ratio) {
  
  data_set <- eval(parse(text = data_name), envir = parent.frame())
  
  # Add data_ID column.
  nrows <- nrow(data_set)
  data_set[[paste0(data_name, "_ID")]] <- 1:nrows
  
  # We remove the rows which contain 'NA' values in the output variable.
  data_set <- data_set[!is.na(data_set[[output_var]]), ]
  
  # Remove columns which aren't needed.
  if (!is.null(drop_columns)) {
    column_names <- colnames(data_set)
    for (column in drop_columns) {
      var_no <- which(column == column_names)[1]
      data_set <- data_set[, -var_no]
    }
  }
  
  # We extract the amount of data points and calculate our training size
  no_training_points <- as.integer(nrows * train_test_ratio)
  
  # Now we can create a training set by selecting an amount of data points
  # according to the train_test_ratio
  training_choice <- sample.int(nrows, no_training_points)
  
  output <- list()
  output[[1]] <- data_set[training_choice, ]
  output[[2]] <- data_set[-training_choice, ]

  return(output)
}