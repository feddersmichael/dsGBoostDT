
#' Retrieve the hessian bins
#'
#' @param data_name The name of the data.
#' @param bounds_and_levels List of maximum and minimum value for numeric and
#' levels for factor features.
#' @param spp_cand The Splitting-point candidates.
#' @param data_classes Data class per feature. 
#'
#' @return The hessian bins.
#' @export
hessiansDS <- function(data_name, bounds_and_levels, spp_cand, data_classes) {
  
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  features <- names(bounds_and_levels)
  numerics <- sapply(features, function(name){return(data_classes[[name]] == "numeric")})
  features <- features[numerics]
  # We also prepare our breaks to cut the data into bins
  breaks <- list()
  for (feature in features) {
    breaks[[feature]] <- c(bounds_and_levels[[feature]][1],
                           spp_cand[[feature]],
                           bounds_and_levels[[feature]][2])
  }
  leaf <- training_data
  split_bin_ref <- list()
  split_bin_hess <- list()
  for (feature in features) {
    split_bin_ref[[feature]] <- addNA(cut(leaf[[feature]],
                                          breaks[[feature]],
                                          include.lowest = TRUE))
    NA_index <- length(levels(split_bin_ref[[feature]]))
    split_bin_hess[[feature]] <- split(leaf$hess, split_bin_ref[[feature]])
    names(split_bin_hess[[feature]])[NA_index] <- "NA"
    split_bin_hess[[feature]] <- lapply(split_bin_hess[[feature]], sum)
  }
  
  return(split_bin_hess)
}