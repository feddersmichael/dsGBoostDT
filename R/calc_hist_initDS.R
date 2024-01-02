
calc_hist_initDS <- function(data_name, loss_function){
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")), 
                   envir = parent.frame())
  
  no_var <- length(colnames(data_set[[1]]))
  data_amt <- nrow(training_data)
  training_features <- data_set[[1]][,1:no_var-1]
  training_output <- data_set[[1]][no_var]
  training_output$pred <- rep(0, data_amt)
  
  if (loss_function == "quadratic"){
    
    training_output$grad <- -2 * training_output[[1]]
    training_output$hess <- rep(-2, data_amt)
  }
  
  return(list(training_features, training_output))
}