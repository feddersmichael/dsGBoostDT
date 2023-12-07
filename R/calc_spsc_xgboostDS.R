
# TODO: What if data is too big to load into the environment all at once?
# TODO: Where is max_min saved
calc_spsc_xgboostDS <- function(data_name, min_max, spp_cand, lambda){
  
  data_sep <- eval(parse(text = paste0(data_name, "_sep")), 
                   envir = parent.frame())
  
  training_features <- data_sep[[1]][[1]]
  training_output <- data_sep[[1]][[2]]
  
  # We also prepare our breaks to cut the data into bins
  breaks <- mapply(c, min_max[1, ], spp_cand, min_max[2, ])
  
  # We start with sorting the data into bins for each feature
  # First we create the bin reference
  split_bin_ref <- mapply(cut, as.list(training_features), 
                          as.list(as.data.frame(breaks)), 
                          MoreArgs = list(labels = FALSE, 
                                          include.lowest = TRUE))
  
  # Then we can create lists containing the elements of the output which would
  # fall in each bin for each splitting in the features
  split_bin <- mapply(split, output, as.list(as.data.frame(split_bin_ref)))
  
  # Now we can calculate the histograms for all bins
  # second derivative of quadratic loss is just 2 so we can skip them
  
  
  
  
  
  
  # Rewrite: need split_up managed
  hist_1_squ <- lapply(X = split_bin, FUN = calc_hist_1_squ)
  
  hist_2_pl_lmbd <- lapply(X = split_bin, FUN = calc_hist_2_pl_lmbd, lambda)
  
  
  
}

calc_hist_1_squ <- function(Y){
  
  output <- sapply(X = Y, FUN = function(Z){return(sum(Z)^2)})
  
  return(output)
}

calc_hist_2_pl_lmbd <- function(Y, lambda){
  
  output <- sapply(X = Y, FUN = function(Z){return(length(Z) + lambda)})
  
  return(output)
}