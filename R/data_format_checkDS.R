
#' Checking the Data for unwanted behaviour.
#'
#' @param data_name Name of the data.
#' @param bounds_and_levels List of all columns which stores either bounds or
#' level sets for all variables.
#' @param output_var The name of the output variable.
#' @param loss_function The loss function which we use to optimize our boosted
#' tree.
#' @param drop_columns Which columns should be dropped from the data.
#' @param drop_NA If NA-values should be deleted.
#'
#' @return The data classes for all columns.
#' @export
data_format_checkDS <- function(data_name, bounds_and_levels, output_var,
                                loss_function, drop_columns, drop_NA) {

  if (!exists(data_name, envir = parent.frame())) {
    stop(paste0("There exists no data saved under the name '", data_name, "'."))
  }

  data_set <- eval(parse(text = data_name), envir = parent.frame())

  if (!is.data.frame(data_set)) {
    stop(paste0("The object saved under the name '", data_name,
                "' has data type '", class(data_set),
                "' instead of 'data frame'."))
  }

  column_names <- colnames(data_set)

  # We remove the 'drop_columns' features from the data
  if (!is.null(drop_columns)) {
    row_numbers <- which(column_names %in% drop_columns)
    if (length(row_numbers) == length(drop_columns)) {
      data_set <- data_set[, -row_numbers]
    } else {
      stop("The columns which shall be removed don't exist.")
    }
  }

  exp_columns <- names(bounds_and_levels)
  if (!identical(sort(exp_columns), sort(colnames(data_set)))) {
    stop("The remaining column names of the data set don't coincide with the expected names.")
  }

  cont_NA <- is.na(data_set[[output_var]])

  if (any(cont_NA)) {
    # currently NA-data needs to be dropped if it exists.
    if (!drop_NA) {
      stop("The data contains 'NA' values in the output variable.")
    } else {
      data_set <- data_set[!cont_NA, ]
    }
  }

  data_classes <- sapply(data_set, data.class)

  for (column in exp_columns) {
    if (data_classes[[column]] == "numeric") {

      if (!is.numeric(bounds_and_levels[[column]]) ||
            length(bounds_and_levels[[column]]) != 2 ||
            (bounds_and_levels[[column]][[1]] > bounds_and_levels[[column]][[2]])) {
        stop("For numeric features 'bounds_and_levels' should be a numeric vector of length 2 with a lower and upper limit for all elements in this column.")
      }
      
      # TODO: what if all data is NA?
      if (min(data_set[[column]], na.rm = TRUE) < bounds_and_levels[[column]][1]) {
        stop(paste0("The values in the column '", column, "' aren't restricted to the expected boundaries."))
      }

      if (max(data_set[[column]], na.rm = TRUE) > bounds_and_levels[[column]][2]) {
        stop(paste0("The values in the column '", column, "' aren't restricted to the expected boundaries."))
      }
    } else if (data_classes[[column]] == "factor") {

      if (!is.character(bounds_and_levels[[column]])) {
        stop("For factor features 'bounds_and_levels' should be a character vector which provides the expected levels.")
      }

      if (!identical(bounds_and_levels[[column]], levels(data_set[[column]]))) {
        stop(paste0("The levels of the column '", column, "' don't coincide with the expected amount or order."))
      }
    } else {
      stop("Each column should be either of data class 'numeric' or 'factor'.")
    }
  }

  if (identical(loss_function, "quadratic")) {
    if (!identical(data_classes[[output_var]], "numeric")) {
      stop(paste0("The loss function 'quadratic' is not suitable for this type of data."))
    }
  } else if (identical(loss_function, "binary_cross_entropy")) {
    if (!identical(data_classes[[output_var]], "numeric")
        || !all(bounds_and_levels[[output_var]] %in% c(0, 1))) {
      stop(paste0("The loss function 'binary_cross_entropy' is not suitable for this type of data."))
    }
  } else if (identical(loss_function, "binary_sigmoid")) {
    if (!identical(data_classes[[output_var]], "numeric")
        || !all(bounds_and_levels[[output_var]] %in% c(0, 1))) {
      stop(paste0("The loss function 'binary_sigmoid' is not suitable for this type of data."))
    }
  }

  return(data_classes)
}
