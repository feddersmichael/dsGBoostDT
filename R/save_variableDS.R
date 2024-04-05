
#' Save a variable
#'
#' @param data_name Name of the data.
#' @param variable The variable which shall be saved.
#'
#' @return The variable which shall be saved.
#' @export
save_variableDS <- function(data_name, variable) {
  
  if (exists(paste0(data_name, "_", variable), envir = parent.frame())) {
    stop("The variable name '", data_name, "_", variable, "'is already in use.")
  }
  return(variable)
}