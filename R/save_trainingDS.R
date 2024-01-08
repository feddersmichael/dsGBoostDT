
#' save_trainingDS
#'
#' @param data_name 
#'
#' @return
#' @export
#'
#' @examples
save_trainingDS <- function(data_name){
  
  data_set <- eval(parse(text = paste0(data_name, "_training_test_split")), 
                   envir = parent.frame())
  
  no_var <- length(colnames(data_set[[1]]))
  
  training_features <- data_set[[1]][,1:no_var-1]
  training_output <- data_set[[1]][no_var]
  
  return(list(training_features, training_output))
}