
# TODO: Where is max_min saved
# TODO: if else step with generating spp and spsc new or recycle old ones
# TODO: A LOT!!! (checks etc)
calc_histDS <- function(data_name, loss_function, data_type, curr_tree, 
                        spp_cand){
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.character(loss_function)){
    stop("'loss_function' needs to have data type 'character'.")
  }
  
  if (!is.character(data_type)){
    stop("'data_type' needs to have data type 'character'.")
  }
  
  if (!is.data.frame(curr_tree)){
    stop("'curr_tree' needs to be an object of type 'data frame'.")
  }
  
  if (!is.list(spp_cand)){
    stop("'spp_cand' needs to be an object of type 'list'.")
  }
  
  # We read in the data from the server and extract the features and output
  # from the training-data.
  data_sep <- eval(parse(text = paste0(data_name, "_sep")), 
                   envir = parent.frame())
  training_features <- data_sep[[1]][[1]]
  training_output <- data_sep[[1]][[2]]
  
  
  
  # We also need the load in the predicted output value of the boosted_tree up
  # to now.
  current_prediction <- cur_predDS()
  
  
  # We also prepare our breaks to cut the data into bins
  if (data_type = "numeric_cont").{
    breaks <- mapply(c, min_max[1, ], spp_cand, min_max[2, ], SIMPLIFY = FALSE)
  }
  
  # We start with sorting the data into bins for each feature
  # First we create the bin reference
  if (data_type = "numeric_cont").{
    split_bin_ref <- mapply(cut, as.list(training_features), breaks, 
                            MoreArgs = list(labels = FALSE, include.lowest = TRUE), 
                            SIMPLIFY = FALSE)
  }
  
  # Then we can create lists containing the elements of the output which would
  # fall in each bin for each splitting in the features
  split_bin_output <- mapply(split, list(training_output[, 1]), split_bin_ref)
  # assuming "current_prediction" is a vector
  # TODO: what happens if no element is in a bin? -> should be 0
  split_bin_pred <- mapply(split, list(current_prediction), split_bin_ref)
  
  # Now we can calculate the histograms for all bins
  
  if (loss_function = "quadratic"){
    help_function_1 <- function(Y, Y_hat){
      
      out <- mapply(function(Z, Z_hat){return(-2 * (sum(Z) - sum(Z_hat)))}, 
                    Y, Y_hat)
      return(out)
    }
    
    help_function_2 <- function(Y){
      
      out <- sapply(Y, function(Z){return(-2 * length(Z))})
      
      return(out)
    }
    
    # TODO: Output has to be list over features and each feature represented
    #  through a vector
    hist_1 <- mapply(help_function_1, split_bin_output, split_bin_pred)
    
    hist_2 <- lapply(split_bin_output, help_function_2)
  }
  
  return(list(hist_1, hist_2))
}