
#' Split up data in training features and training output
#'
#' @param data_name Name of the data.
#'
#' @return The split.
#' @export
save_trainingDS <- function(data_name){
  
  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")), 
                   envir = parent.frame())
  
  no_var <- length(colnames(data_set[[1]]))
  
  training_features <- data_set[[1]][,1:no_var-1]
  training_output <- data_set[[1]][no_var]
  
  return(list(training_features, training_output))
}