
#' Calculate the predicted output and histograms
#'
#' @param data_name Name of the data.
#' @param output_var The name of the column containing the output.
#' @param loss_function Type of loss function under which the tree is optimised.
#'
#' @return The training features and calculated output and histograms.
#' @export
calc_hist_initDS <- function(data_name, output_var, loss_function) {
  # TODO: prediction initialization -> hyper parameter optimization?
  #       could be done with average

  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")),
                   envir = parent.frame())

  training_data <- data_set[[1]]
  data_amt <- nrow(training_data)

  if (loss_function == "quadratic") {
    training_data$pred <- rep(0, data_amt)
    training_data$grad <- -2 * training_data[[output_var]]
    training_data$hess <- rep(-2, data_amt)
  } 
  else if (loss_function == "binary_cross_entropy") {
    training_data$pred <- rep(0.5, data_amt)
    training_data$grad <- -4 * training_data[[output_var]] + 2
    training_data$hess <- rep(4, data_amt)
  }

  return(training_data)
}