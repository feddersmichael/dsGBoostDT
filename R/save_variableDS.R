
#' Save a variable
#'
#' @param data_name Name of the data.
#' @param variable_name Name of the variable which we want to save.
#' @param variable The variable which shall be saved.
#'
#' @return The variable which shall be saved.
#' @export
save_variableDS <- function(data_name, variable_name, variable) {
  
  if (!is.null(variable_name) &&
      exists(paste0(data_name, "_", variable_name), envir = parent.frame())) {
    stop("The variable name '", data_name, "_", variable_name,
         "'is already in use.")
  }
  
  return(variable)
}