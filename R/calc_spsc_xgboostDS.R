
# TODO: What if data is too big to load into the environment all at once?
# TODO: Where is max_min saved
calc_spsc_xgboostDS <- function(data_name, min_max, spp_cand, lambda){
  
  data_sep <- eval(parse(text = paste0(data_name, "_sep")), 
                   envir = parent.frame())
  
  training_features <- data_sep[[1]][[1]]
  
  # We also prepare our breaks to cut the data into bins
  breaks <- mapply(c, min_max[1, ], spp_cand, min_max[2, ])
  
  # TODO: wollen durch spp_cand gehen und dann jeweils score berechnen
  # Wie geht das effizient?
  
  # We start with sorting the data into bins for each feature
  # First we create the bin reference
  split_bin_ref <- mapply(cut, as.list(training_features), 
                          as.list(as.data.frame(breaks)), 
                          MoreArgs = list(labels = FALSE, 
                                          include.lowest = TRUE))
  
  # Then we can create lists containing the elements which fall in each bin
  
  
  # Now we can calculate the histograms for all bins
  
  
}