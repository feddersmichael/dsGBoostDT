
#' Calculate the histograms.
#'
#' @param data_name Name of the data.
#' @param last_tr_tree The last tree which was trained.
#' @param loss_function The type of loss-function under which we optimizes our
#' boosted tree.
#'
#' @return The training features and calculated output and histograms.
#' @export
calc_histDS <- function(data_name, last_tr_tree, loss_function){
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.data.frame(last_tr_tree)){
    stop("'last_tr_tree' needs to be an object of type 'data frame'.")
  }
  
  # We read in the training-data from the server and update the
  # output-prediction.
  training_data <- eval(parse(text = paste0(data_name, "_training")), 
                   envir = parent.frame())
  
  training_data[[2]]$pred <- training_data[[2]]$pred +
                             apply(X = training_data[[1]], MARGIN = 1, 
                                     FUN = tree_evaluationDS, last_tr_tree)
  
  if (loss_function == "quadratic"){
    training_data[[2]]$grad <- -2 * (training_data[[2]][[1]] - 
                                       training_data[[2]]$pred)
  }
  else if (loss_function == "binary_cross_entropy"){
    output <- training_data[[1]]
    prediction <- training_data[[2]]$pred
    pred_1 <- prediction - 1
    training_data[[2]]$grad <- (output - prediction) / 
      (pred_1 * prediction)
    training_data[[2]]$hess <- output / prediction^2 - (output - 1) / pred_1^2
  }
  
  return(training_data)
}