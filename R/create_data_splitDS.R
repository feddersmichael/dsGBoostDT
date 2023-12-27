
create_data_splitDS <- function(data_name, train_test_ratio, split_status){
  
  # We first check all the inputs for appropriate class
  if (!is.numeric(train_test_ratio) || !(0 <= numeric(train_test_ratio) <= 1)){
    stop(paste0("'train_test_ratio' needs to have data type 'numeric' and lie",
    " between 0 and 1."))
  }
  
  if (is.null(split_status)){
    if (!is.character(data_name) || !(length(data_name) != 1)){
      stop("'data_name' needs to be an atomic vector with data type 'character'.")
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
  }
  else if (split_status == 'Train'){
    if (!is.character(data_name) || !(length(data_name) != 1)){
      stop("'data_name' needs to be an atomic vector with data type 'character'.")
    }
    
    train_set <- eval(parse(text = data_name), envir = parent.frame())
    
    output <- list()
    output[[1]] <- train_set
    output[[2]] <- NULL
  }
  else if (split_status == 'Train_Test'){
    if (!is.character(data_name) || !(length(data_name) != 2)){
      stop("'data_name' needs to be an object of type 'vector' with data type 'character' and length 2.")
    }
    
    train_set <- eval(parse(text = data_name[1]), envir = parent.frame())
    test_set <- eval(parse(text = data_name[2]), envir = parent.frame())
    
    output <- list()
    output[[1]] <- train_set
    output[[2]] <- test_set
  }
  else {
    stop("'split_status' needs to be either 'NULL' or an atomic vector with data type 'character'.")
  }
  
  return(output)
}