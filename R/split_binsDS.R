
#' Split up the data into bins by slitting points.
#'
#' @param data_name The name of the data.
#' @param bounds_and_levels List of maximum and minimum value for numeric and
#' levels for factor features.
#' @param spp_cand The Splitting-point candidates.
#' @param current_tree The currently trained tree.
#' @param data_type The type of data per feature.
#'
#' @return The histogram bins for each split in all features.
#' @export
split_binsDS <- function(data_name, bounds_and_levels, spp_cand, current_tree, data_type){
  
  # TODO: Just save data which remains after each split on the server.
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }

  if (!is.list(spp_cand)){
    stop("'spp_cand' needs to be an object of type 'list'.")
  }
  
  # We read in the data from the server and extract the features, output and 
  # predicted output from the training data
  training_data <- eval(parse(text = paste0(data_name, "_training")), 
                   envir = parent.frame())
  
  amt_features <- ncol(training_features)
  
  
  
  # We only need to calculate the histogram-bins for the last two added leaves.
  # Therefore we reduce the data to the rows which are contained in the two
  # leaves.
  
  leaves <- data_splitDS(training_data, bounds_and_levels, current_tree)
  
  # We also prepare our breaks to cut the data into bins
  breaks <- mapply(c, bounds_and_levels[1, ], spp_cand, bounds_and_levels[2, ], SIMPLIFY = FALSE)
  
  histograms <- list()
  
  for (leaf in leaves){
    # We start with sorting the data into bins for each feature
    # First we create the bin reference
    split_bin_ref <- mapply(cut, as.list(leaf[[1]][, c(2:amt_features)]),
                            breaks, MoreArgs = list(include.lowest = TRUE), 
                            SIMPLIFY = FALSE)
    # We give 'NA' values an own category
    add_NA_cat <- function(bin_vector, amt_cat){
      bin_vector[is.na(bin_vector)] <- amt_cat + 1
      return(bin_vector)
    }
    amt_cat <- lapply(spp_cand[data_type], length)
    split_bin_ref[data_type] <- mapply(add_NA_cat, split_bin_ref[data_type],
                                       amt_cat)
    
    split_bin_grad <- mapply(split, list(leaf[[2]]$grad), split_bin_ref)
    split_bin_hess <- mapply(split, list(leaf[[2]]$hess), split_bin_ref)
    histograms <- append(histograms, list(split_bin_grad, split_bin_hess))
  }
  
  return(histograms)
}