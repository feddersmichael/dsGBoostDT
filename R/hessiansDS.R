
#' Retrieve the hessian bins
#'
#' @param data_name The name of the data.
#' @param training_data The data which we use to build the tree.
#' @param bounds_and_levels Bounds for numeric columns and levels for factors.
#' @param data_classes Data class for all features.
#' @param spp_cand The split-point candidates for which the bins were
#' calculated.
#' @param selected_feat Which part of the feature space we use to build
#'
#' @return The hessian bins.
#' @export
hessiansDS <- function(data_name, training_data = NULL,
                       bounds_and_levels = NULL, data_classes = NULL,
                       spp_cand = NULL, selected_feat = NULL) {
  
  if (is.null(training_data)) {
    training_data <- eval(parse(text = paste0(data_name, "_training")),
                          envir = parent.frame())
  }
  if (is.null(bounds_and_levels)) {
    bounds_and_levels <- eval(parse(text = paste0(data_name, "_bounds_and_levels")),
                              envir = parent.frame())
  }
  if (is.null(data_classes)) {
    data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                         envir = parent.frame())
  }
  if (is.null(spp_cand)) {
    spp_cand <- eval(parse(text = paste0(data_name, "_spp_cand")),
                     envir = parent.frame())
  }
  if (is.null(selected_feat)) {
    selected_feat <- eval(parse(text = paste0(data_name, "_selected_feat")),
                          envir = parent.frame())
  }
  
  
  numerics <- sapply(selected_feat, function(name){return(data_classes[[name]] == "numeric")})
  selected_feat <- selected_feat[numerics]
  # We also prepare our breaks to cut the data into bins
  breaks <- list()
  for (feature in selected_feat) {
    breaks[[feature]] <- c(bounds_and_levels[[feature]][1],
                           spp_cand[[feature]],
                           bounds_and_levels[[feature]][2])
  }
  leaf <- training_data
  split_bin_ref <- list()
  split_bin_hess <- list()
  for (feature in selected_feat) {
    split_bin_ref[[feature]] <- addNA(cut(leaf[[feature]],
                                          breaks[[feature]],
                                          include.lowest = TRUE))
    NA_index <- length(levels(split_bin_ref[[feature]]))
    split_bin_hess[[feature]] <- split(leaf$hess, split_bin_ref[[feature]])
    names(split_bin_hess[[feature]])[NA_index] <- "NA"
    split_bin_hess[[feature]] <- sapply(split_bin_hess[[feature]], sum)
  }
  
  return(split_bin_hess)
}