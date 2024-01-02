
# TODO: NA data
split_binsDS <- function(data_name, min_max, spp_cand, current_tree){
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.list(min_max)){
    stop("'min_max' needs to be an object of type 'list'.")
  }

  if (!is.list(spp_cand)){
    stop("'spp_cand' needs to be an object of type 'list'.")
  }

  
  # We read in the data from the server and extract the features, output and 
  # predicted output from the training data
  training_data <- eval(parse(text = paste0(data_name, "_training")), 
                   envir = parent.frame())
  
  data_amt <- nrow(training_data)
  training_features <- training_data[[1]]
  training_output <- training_data[[2]]
  
  # We only need to calculate the histogram-bins for the last two added leaves.
  # Therefore we reduce the data to the rows which are contained in the two
  # leaves.
  amt_spp <- nrow(current_tree)
  # If the tree is empty we use the whole data for the splits.
  if (amt_spp = 0){
    leaves <- list(training_features)
  }
  else {
    leaves <- data_splitDS(training_features, min_max, current_tree)
  }
  
  
  
  
  
  # We also prepare our breaks to cut the data into bins
  breaks <- mapply(c, min_max[1, ], spp_cand, min_max[2, ], SIMPLIFY = FALSE)
  
  # We start with sorting the data into bins for each feature
  # First we create the bin reference
  split_bin_ref <- mapply(cut, as.list(training_features), breaks, 
                            MoreArgs = list(labels = FALSE, 
                                            include.lowest = TRUE), 
                            SIMPLIFY = FALSE)
  
  # TODO: what happens if no element is in a bin? -> should be 0
  split_bin_grad <- mapply(split, list(training_grad), split_bin_ref)
  split_bin_hess <- mapply(split, list(training_hess), split_bin_ref)
  
  return(list(split_bin_grad, split_bin_hess))
}