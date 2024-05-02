
#' Calculate the histograms.
#'
#' @param data_name Name of the data.
#' @param amt_trees How many trees have been built already.
#' @param removed_trees Which trees got removed for training.
#'
#' @return The training features and calculated output and histograms.
#' @export
calc_histDS <- function(data_name, amt_trees, removed_trees = NULL) {

  # We first check all the inputs for appropriate class
  if (!is.character(data_name)) {
    stop("'data_name' needs to have data type 'character'.")
  }
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  output_var <- eval(parse(text = paste0(data_name, "_output_var")),
                     envir = parent.frame())
  loss_function <- eval(parse(text = paste0(data_name, "_loss_function")),
                        envir = parent.frame())
  prediction <- training_data$full_tree
  for (number in removed_trees) {
    cur_tree <- eval(parse(text = paste0(data_name, "_tree_", number)),
                     envir = parent.frame())
    prediction <- prediction - cur_tree
  }
  training_data$pred <- prediction
  output <- training_data[[output_var]]
  
  if (loss_function == "quadratic") {
    difference <- output - prediction
    training_data$loss <- difference^2
    training_data$grad <- -2 * difference
  } else if (loss_function == "binary_cross_entropy") {
    pred_1 <- prediction - 1
    log_pred <- log(-1 * pred_1)
    training_data$loss <-  output * (log_pred - log(prediction)) - log_pred
    val_per_pred <- output / prediction
    va_1_per_pred_1 <- (output - 1) / pred_1
    training_data$grad <- va_1_per_pred_1 - val_per_pred
    training_data$hess <- val_per_pred / prediction - va_1_per_pred_1 / pred_1
  } else if (loss_function == "binary_sigmoid") {
    # If we happen to get absolute prediction values over 6 we cut it off at
    # that value to prevent overflow
    prediction[prediction > 6] <- 6
    prediction[prediction < -6] <- -6
    exp_z <- exp(prediction)
    one_pl_exp_z <- exp_z + 1
    training_data$loss <- log(one_pl_exp_z) - prediction / (output - 1)
    exp_relation <- exp_z / one_pl_exp_z
    training_data$grad <- exp_relation - output
    training_data$hess <- exp_relation / one_pl_exp_z
  }
  return(training_data)
}