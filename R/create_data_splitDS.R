
create_data_splitDS <- function(data_name, train_test_ratio){
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.numeric(train_test_ratio) || !(0 <= numeric(train_test_ratio) <= 1)){
    stop(paste0("'train_test_ratio' needs to have data type 'numeric' and lie",
    " between 0 and 1."))
  }
    
  
  data_set <- eval(parse(text = data_name), envir = parent.frame())
  
  # We extract the amount of data points and calculate our training size
  nrows <- nrow(data_set)
  no_training_points <- as.integer(nrows * train_test_ratio)
  
  # Now we can create a training set by selecting an amount of data points
  # according to the train_test_ratio
  training_choice <- sample.int(nrows, no_training_points)
  
  output <- list()
  output[[1]] <- data_set[training_choice, ]
  output[[2]] <- data_set[-training_choice, ]
  
  return(output)
}