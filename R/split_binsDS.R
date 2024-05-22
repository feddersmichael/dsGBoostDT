
#' Split up the data into bins by slitting points.
#'
#' @param data_name The name of the data.
#' @param leaves_list The list of all leaves, input if training on server.
#'
#' @return The histogram bins for each split in all features.
#' @export
split_binsDS <- function(data_name, leaves_list = NULL) {
  # We read in the data from the server and extract the features, output and
  # predicted output from the training data
  if (is.null(leaves_list)) {
    leaves_list <- eval(parse(text = paste0(data_name, "_leaves")),
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
  
  bounds_and_levels <- eval(parse(text = paste0(data_name, "_bounds_and_levels")),
                            envir = parent.frame())
  data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                       envir = parent.frame())
  spp_cand <- eval(parse(text = paste0(data_name, "_spp_cand")),
                       envir = parent.frame())
  selected_feat <- eval(parse(text = paste0(data_name, "_selected_feat")),
                        envir = parent.frame())
  
  if (is.null(selected_feat)) {
    features <- names(data_classes)
  } else {
    features <- selected_feat
  }
  
  # We also prepare our breaks to cut the data into bins
  breaks <- list()
  for (feature in features) {
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

    for (feature in features) {
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

      split_bin_grad[[feature]] <- lapply(split_bin_grad[[feature]], sum)
      split_bin_hess[[feature]] <- lapply(split_bin_hess[[feature]], sum)
    }

    histograms[[i]] <- list(grad = split_bin_grad, hess = split_bin_hess)
  }

  return(histograms)
}