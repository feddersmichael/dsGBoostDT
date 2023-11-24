
# TODO: Some checks regarding min max etc
create_data_splitDS <- function(data_name, train_test_ratio, seed = NULL){
  
  data_set <- eval(parse(text = data_name), envir = parent.frame())
  
  # if a seed is given we initialize it
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  # We extract the amount of data points and calculate our training size
  nrows <- nrow(data_set)
  no_training_points <- as.integer(nrows * train_test_ratio)
  
  # Now we can create a training set by selecting an amount of data points
  # according to the train_test_ratio
  training_choice <- sample.int(nrows, no_training_points, replace = TRUE)
  
  output <- list()
  output[[1]] <- data_set[training_choice, ]
  output[[2]] <- data_set[-training_choice, ]
  
  return(output)
}