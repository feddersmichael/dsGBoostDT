
output_pred_initDS <- function(data_name){
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  
  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")), 
                   envir = parent.frame())
  
  no_var <- length(colnames(data_set[[1]]))
  training_features <- data_set[[1]][,1:no_var-1]
  training_output <- data_set[[1]][no_var]
  training_pred <- data.frame(output_pred = rep(0, nrow(training_data)))
  
  return(list(training_features, training_output, training_pred))
}