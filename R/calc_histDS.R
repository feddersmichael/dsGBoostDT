
#' Calculate the histograms.
#'
#' @param data_name Name of the data.
#' @param last_tr_tree The last tree which was trained.
#' @param data_classes Data class for all features.
#' @param output_var The name of the column containing the output.
#' @param loss_function The type of loss-function under which we optimizes our
#' boosted tree.
#'
#' @return The training features and calculated output and histograms.
#' @export
calc_histDS <- function(data_name, last_tr_tree, data_classes, output_var,
                        loss_function) {

  # We first check all the inputs for appropriate class
  if (!is.character(data_name)) {
    stop("'data_name' needs to have data type 'character'.")
  }

  if (!is.data.frame(last_tr_tree)) {
    stop("'last_tr_tree' needs to be an object of type 'data frame'.")
  }

  # We read in the training-data from the server and update the
  # output-prediction.
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())

  data_by_row <- split(training_data, seq_len(nrow(training_data)))

  training_data$pred <- sapply(X = data_by_row, FUN = tree_evaluationDS,
                               last_tr_tree, data_classes) + training_data$pred

  output <- training_data[[output_var]]
  prediction <- training_data$pred

  if (loss_function == "quadratic") {
    difference <- output - prediction
    training_data$loss <- difference^2
    training_data$grad <- -2 * difference
  } else if (loss_function == "binary_cross_entropy") {
    outp_zero <- output == 0
    outp_one <- output == 1
    loss_zero <- log(-(training_data$pred[outp_zero] - 1))
    loss_one <- log(training_data$pred[outp_one])
    training_data$loss <- rep(0, data_amt)
    training_data$loss[outp_zero] <- loss_zero
    training_data$loss[outp_one] <- loss_one
    
    pred_1 <- prediction - 1
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
    exp_relation <- exp_z / one_pl_exp_z
    training_data$grad <- exp_relation - output
    training_data$hess <- exp_relation / one_pl_exp_z
  }

  return(training_data)
}