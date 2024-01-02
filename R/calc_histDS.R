
calc_histDS <- function(data_name, last_tr_tree){
  
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
  
  return(training_data)
}