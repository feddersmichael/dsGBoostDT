
#' Checking the Data for unwanted behaviour.
#'
#' @param data_name Name of the data.
#' @param bounds_and_levels List of all columns which stores either bounds or
#' level sets for all variables.
#' @param output_var The name of the output variable.
#' @param loss_function The loss function which we use to optimize our boosted
#' tree.
#' @param drop_NA If NA-values should be deleted.
#'
#' @return The data classes for all columns.
#' @export
data_format_checkDS <- function(data_name, bounds_and_levels, output_var,
                                loss_function, drop_NA) {
  #TODO: Possibility to overwrite levels for factors.
  
  if (!exists(data_name)) {
    stop(paste0("There exists no data saved under the name '", data_name, "'."))
  }
  
  data_set <- eval(parse(text = data_name), envir = parent.frame())

  
  if (!is.data.frame(data_set)) {
    stop(paste0("The object saved under the name '", data_name, "' has data type '",
                class(data_set), "' instead of 'data frame'."))
  }
  
  exp_columns <- names(bounds_and_levels)
  if (!identical(exp_columns, colnames(data_set))) {
    stop("The column names of the data set don't coincide with the expected names.")
  }
  
  cont_NA <- !is.na(data_set[[output_var]])
  
  
  
  if (!all(cont_NA)) {
    if (drop_NA) {
      warning("The data contains 'NA' values in the output variable.")
      # data_set <- data_set[cont_NA, ]
    } else {
      stop("The data contains 'NA' values in the output variable.")
    }
  }
  
  data_classes <- sapply(data_set, data.class)
  
  for (i in 1:length(exp_columns)) {
    if (data_classes[[i]] == "numeric") {
      if (min(data_set[[i]], na.rm = TRUE) < bounds_and_levels[[i]][1]) {
        stop(paste0("The values in the column '", exp_columns[i], "' aren't restricted to the expected boundaries."))
      }
      
      if (max(data_set[[i]], na.rm = TRUE) > bounds_and_levels[[i]][2]) {
        stop(paste0("The values in the column '", exp_columns[i], "' aren't restricted to the expected boundaries."))
      }
    }
    else if (data_classes[[i]] == "factor") {

      if (!identical(bounds_and_levels[[i]], levels(data_set[[i]]))) {
        stop(paste0("The levels of the column '", exp_columns[i], "' don't coincide with the expected amount or order."))
      }
    }
    else {
      stop("Each column should be either of data class 'numeric' or 'factor'.")
    }
  }
  
  if (identical(loss_function, "quadratic")) {
    if (!identical(data.class(data_set[[output_var]]), "numeric")) {
      stop(paste0("The loss function 'quadratic' is not suitable for this type of data."))
    }
  } 
  else if (identical(loss_function, "binary_cross_entropy")) {
    if (!identical(data.class(data_set[[output_var]]), "numeric")
        || !identical(bounds_and_levels[[output_var]], c(0, 1))) {
      stop(paste0("The loss function 'binary_cross_entropy' is not suitable for this type of data."))
    }
  }
  
  return(data_classes)
}

