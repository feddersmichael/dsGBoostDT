
#' Calculate the predicted output and histograms
#'
#' @param data_name Name of the data.
#' @param loss_function Type of loss function under which the tree is optimised.
#'
#' @return The training features and calculated output and histograms.
#' @export
calc_hist_initDS <- function(data_name, loss_function){
  # TODO: prediction initialization -> hyper parameter optimization?
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")), 
                   envir = parent.frame())
  
  no_var <- length(colnames(data_set[[1]]))
  training_features <- data_set[[1]][,1:no_var-1]
  training_output <- data_set[[1]][no_var]
  data_amt <- nrow(training_features)
  
  if (loss_function == "quadratic"){
    
    training_output$pred <- rep(0, data_amt)
    training_output$grad <- -2 * training_output[[1]]
    training_output$hess <- rep(-2, data_amt)
  }
  else if (loss_function == "binary_cross_entropy"){
    training_output$pred <- rep(0.5, data_amt)
    training_output$grad <- -4 * training_output[[1]] + 2
    training_output$hess <- rep(4, data_amt)
  }
  
  return(list(training_features, training_output))
}