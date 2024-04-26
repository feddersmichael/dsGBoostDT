
#' Calculate the predicted output and histograms
#'
#' @param data_name Name of the data.
#' 
#' @return The training features and calculated output and histograms.
#' @export
  calc_hist_initDS <- function(data_name) {
  # TODO: prediction initialization -> hyper parameter optimization?
  #       could be done with average
  
  output_var <- eval(parse(text = paste0(data_name, "_output_var")),
                     envir = parent.frame())
  weight_update <- eval(parse(text = paste0(data_name, "_weight_update")),
                     envir = parent.frame())
  loss_function <- eval(parse(text = paste0(data_name, "_loss_function")),
                        envir = parent.frame())
  dropout_rate <- eval(parse(text = paste0(data_name, "_dropout_rate")),
                          envir = parent.frame())
  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")),
                   envir = parent.frame())
  
  training_data <- data_set[["Train"]]
  data_amt <- nrow(training_data)
  
  if (loss_function == "quadratic") {
    if (dropout_rate < 1) {
      training_data$full_tree <- rep(0, data_amt)
    }
    training_data$pred <- rep(0, data_amt)
    training_data$loss <- training_data[[output_var]]^2
    training_data$grad <- -2 * training_data[[output_var]]
    training_data$hess <- rep(2, data_amt)
  } else if (loss_function == "binary_cross_entropy") {
    if (dropout_rate < 1) {
      training_data$full_tree <- rep(0.5, data_amt)
    }
    if (weight_update == "hessian") {
      training_data$pred <- rep(0.5, data_amt)
    } else if (weight_update == "average") {
      training_data$pred <- rep(0, data_amt)
    }
    training_data$loss <- rep(-log(0.5), data_amt)
    training_data$grad <- -4 * training_data[[output_var]] + 2
    training_data$hess <- rep(4, data_amt)
  } else if (loss_function == "binary_sigmoid") {
    if (dropout_rate < 1) {
      training_data$full_tree <- rep(0, data_amt)
    }
    training_data$pred <- rep(0, data_amt)
    training_data$loss <- rep(log(2), data_amt)
    training_data$grad <- -1 * training_data[[output_var]] + 0.5
    training_data$hess <- rep(0.25, data_amt)
  }

  return(training_data)
}