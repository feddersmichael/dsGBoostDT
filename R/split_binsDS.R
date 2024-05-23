
#' Split up the data into bins by slitting points.
#'
#' @param data_name The name of the data.
#' @param leaves_list The list of all leaves, input if training on server.
#' @param bounds_and_levels Bounds for numeric columns and levels for factors.
#' @param data_classes Data class for all features.
#' @param spp_cand The split-point candidates for which the bins were
#' calculated.
#' @param selected_feat Which part of the feature space we use to build
#' the tree.
#'
#' @return The histogram bins for each split in all features.
#' @export
split_binsDS <- function(data_name, leaves_list = NULL,
                         bounds_and_levels = NULL, data_classes = NULL,
                         spp_cand = NULL, selected_feat = NULL) {
  # We read in the data from the server and extract the features, output and
  # predicted output from the training data
  if (is.null(leaves_list)) {
    leaves_list <- eval(parse(text = paste0(data_name, "_leaves")),
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

  # We only need to calculate the histogram-bins for the last two added leaves.
  amt_leaves <- length(leaves_list)
  if (amt_leaves == 1) {
    leaves <- list(leaves_list[[1]])
  } else {
    leaves <- list(leaves_list[[amt_leaves - 1]],
                   leaves_list[[amt_leaves]])
  }
  
  # We also prepare our breaks to cut the data into bins
  breaks <- list()
  for (feature in selected_feat) {
    if (data_classes[[feature]] == "numeric") {
      breaks[[feature]] <- c(bounds_and_levels[[feature]][1],
                             spp_cand[[feature]],
                             bounds_and_levels[[feature]][2])
    } else {
      breaks[[feature]] <- c(1, spp_cand[[feature]],
                             length(bounds_and_levels[[feature]]))
    }
  }
  
  histograms <- list()
  for (i in seq_along(leaves)){

    leaf <- leaves[[i]]
    # We start with sorting the data into bins for each feature
    # First we create the bin reference

    split_bin_ref <- list()
    split_bin_grad <- list()
    split_bin_hess <- list()

    for (feature in selected_feat) {
      if (data_classes[[feature]] == "numeric") {
        split_bin_ref[[feature]] <- addNA(cut(leaf[[feature]],
                                              breaks[[feature]],
                                              include.lowest = TRUE))
        NA_index <- length(levels(split_bin_ref[[feature]]))
        split_bin_grad[[feature]] <- split(leaf$grad, split_bin_ref[[feature]])
        names(split_bin_grad[[feature]])[NA_index] <- "NA"
        split_bin_hess[[feature]] <- split(leaf$hess, split_bin_ref[[feature]])
        names(split_bin_hess[[feature]])[NA_index] <- "NA"
      } else {
        split_bin_ref[[feature]] <- cut(as.numeric(leaf[[feature]]),
                                        breaks[[feature]], include.lowest = TRUE)
        split_bin_grad[[feature]] <- split(leaf$grad, split_bin_ref[[feature]])
        split_bin_hess[[feature]] <- split(leaf$hess, split_bin_ref[[feature]])
      }

      split_bin_grad[[feature]] <- sapply(split_bin_grad[[feature]], sum)
      split_bin_hess[[feature]] <- sapply(split_bin_hess[[feature]], sum)
    }

    histograms[[i]] <- list(grad = split_bin_grad, hess = split_bin_hess)
  }

  return(histograms)
}