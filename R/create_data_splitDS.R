
#' Separate Training and Test data
#'
#' @param data_name Name of the data.
#' @param output_var The name of the column containing the output.
#' @param drop_columns Which columns should be removed from the data.
#' @param train_test_ratio Ratio of training data of the whole data.
#'
#' @return The training-test split.
#' @export
create_data_splitDS <- function(data_name, output_var, drop_columns,
                                train_test_ratio) {

  data_set <- eval(parse(text = data_name), envir = parent.frame())

  # We remove the rows which contain 'NA' values in the output variable.
  data_set <- data_set[!is.na(data_set[[output_var]]), ]

  # Remove columns which aren't needed.
  if (!is.null(drop_columns)) {
    row_numbers <- which(colnames(data_set) %in% drop_columns)
    if (length(row_numbers) == length(drop_columns)) {
      data_set <- data_set[, -row_numbers]
    } else {
      stop("The columns which shall be removed don't exist.")
    }
  }

  # We extract the amount of data points and calculate our training size
  nrows <- nrow(data_set)
  no_training_points <- as.integer(nrows * train_test_ratio)

  # Now we can create a training set by selecting an amount of data points
  # according to the train_test_ratio
  training_choice <- sample.int(nrows, no_training_points)

  output <- list()
  output[["Train"]] <- data_set[training_choice, ]
  output[["Test"]] <- data_set[-training_choice, ]

  return(output)
}