
# TODO: error checking for mode
# ncols is probably know a priori
sep_variablesDS <- function(data_name){
  
  data_train_test <- eval(parse(text = paste0(data_name, "_training_test_split")), envir = parent.frame())
  column_names <- eval(parse(text = 'column_names'), envir = parent.frame())
  
  no_var <- length(column_names)
  
  output <- list()
  data_training <- list()
  data_test <- list()
  
  data_training[[1]] <- data_train_test[[1]][,1:no_var-1]
  data_training[[2]] <- data_train_test[[1]][no_var]
  
  output[[1]] <- data_training
  
  data_test[[1]] <- data_train_test[[2]][,1:no_var-1]
  data_test[[2]] <- data_train_test[[2]][no_var]
  
  output[[2]] <- data_test
  
  return(output)
}