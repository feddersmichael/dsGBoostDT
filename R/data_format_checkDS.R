
data_format_checkDS <- function(data_name, bounds_and_levels, output_var,
                                loss_function, drop_NA) {
  #TODO: Possibility to overwrite levels for factors.
  
  if (!exists(data_name)) {
    stop(paste0("There exists no data saved under the name '", data_name, "'."))
  }
  
  data_set <- eval(parse(data_name), envir = parent.frame())
  
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
    if (is.numeric(data_classes[i])) {
      if (min(data_set[[i]], na.rm = TRUE) < bounds_and_levels[[i]][1]) {
        stop(paste0("The values in the column '", exp_columns[i], "' aren't restricted to the expected boundaries."))
      }
      
      if (max(data_set[[i]], na.rm = TRUE) > bounds_and_levels[[i]][2]) {
        stop(paste0("The values in the column '", exp_columns[i], "' aren't restricted to the expected boundaries."))
      }
    }
    
    if (is.factor(data_classes[i])) {
      if (!identical(bounds_and_levels[[i]], levels(data_set[[i]]))) {
        stop(paste0("The levels of the column '", exp_columns[i], "' don't coincide with the expected amount or order."))
      }
    }
  }
  
  if (identical(loss_function, "quadratic")) {
    if (!identical(data.class(data_set[[output_var]]), "numeric")) {
      stop(paste0("The loss function '", loss_function, "' is not suitable for this type of data."))
    }
  }
  else if (identical(loss_function, "binary_cross_entropy")) {
    if (!identical(data.class(data_set[[output_var]]), "numeric")
        || !identical(bounds_and_levels[[output_var]], c(-1, 1))) {
      stop(paste0("The loss function '", loss_function, "' is not suitable for this type of data."))
    }
  }
  
  return(data_classes)
}