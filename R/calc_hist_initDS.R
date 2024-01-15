
#' Calculate the predicted output and histograms
#'
#' @param data_name Name of the data.
#' @param loss_function Type of loss function under which the tree is optimised.
#' @param output_var The name of the column containing the output.
#' 
#' @return The training features and calculated output and histograms.
#' @export
calc_hist_initDS <- function(data_name, loss_function, output_var) {
  # TODO: prediction initialization -> hyper parameter optimization?
  # TODO: maybe check if ID also needed for output

  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")),
                   envir = parent.frame())
  
  column_names <- colnames(data_set[[1]])
  output_nmbr <- which(column_names == output_var)[1]
  
  training_features <- data_set[[1]][, -output_nmbr]
  training_output <- data_set[[1]][output_nmbr]
  data_amt <- nrow(training_features)

  if (loss_function == "quadratic") {

    training_output$pred <- rep(0, data_amt)
    training_output$grad <- -2 * training_output[[1]]
    training_output$hess <- rep(-2, data_amt)
  } else if (loss_function == "binary_cross_entropy") {
    training_output$pred <- rep(0.5, data_amt)
    training_output$grad <- -4 * training_output[[1]] + 2
    training_output$hess <- rep(4, data_amt)
  }

  return(list(training_features, training_output))
}