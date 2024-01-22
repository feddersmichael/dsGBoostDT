
#' Split up the data into bins by slitting points.
#'
#' @param data_name The name of the data.
#' @param bounds_and_levels List of maximum and minimum value for numeric and
#' levels for factor features.
#' @param spp_cand The Splitting-point candidates.
#' @param current_tree The currently trained tree.
#' @param data_classes Data class per feature.
#'
#' @return The histogram bins for each split in all features.
#' @export
split_binsDS <- function(data_name, bounds_and_levels, spp_cand, current_tree,
                         data_classes) {
  # TODO: Just save data which remains after each split on the server.

  # We first check all the inputs for appropriate class
  if (!is.character(data_name)) {
    stop("'data_name' needs to have data type 'character'.")
  }

  if (!is.list(spp_cand)) {
    stop("'spp_cand' needs to be an object of type 'list'.")
  }

  # We read in the data from the server and extract the features, output and
  # predicted output from the training data
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())

  # We only need to calculate the histogram-bins for the last two added leaves.
  # Therefore we reduce the data to the rows which are contained in the two
  # leaves.

  leaves <- data_splitDS(training_data, bounds_and_levels, current_tree,
                         data_classes)

  features <- names(bounds_and_levels)
  # We also prepare our breaks to cut the data into bins
  breaks <- list()
  for (feature in features) {

    if (data_classes[[feature]] == "numeric") {
      breaks[[feature]] <- c(bounds_and_levels[[feature]][1],
                             spp_cand[[feature]],
                             bounds_and_levels[[feature]][2])
    }
    else {
      breaks[[feature]] <- c(1, spp_cand[[feature]],
                             length(bounds_and_levels[[feature]]))
    }
  }
  
  histograms <- list()
  
  for (i in 1:length(leaves)){

    leaf <- leaves[[i]]
    # We start with sorting the data into bins for each feature
    # First we create the bin reference

    split_bin_ref <- list()
    split_bin_grad <- list()
    split_bin_hess <- list()

    for (feature in features) {

      if (data_classes[[feature]] == "numeric") {
        if (anyNA(leaf[[feature]])) {
          split_bin_ref[[feature]] <- addNA(cut(leaf[[feature]], breaks[[feature]],
                                                include.lowest = TRUE))

          NA_index <- length(levels(split_bin_ref[[feature]]))
          split_bin_grad[[feature]] <- split(leaf$grad, split_bin_ref[[feature]])
          names(split_bin_grad[[feature]])[NA_index] <- "NA"
          split_bin_hess[[feature]] <- split(leaf$hess, split_bin_ref[[feature]])
          names(split_bin_hess[[feature]])[NA_index] <- "NA"
        }
        else {
          split_bin_ref[[feature]] <- cut(leaf[[feature]], breaks[[feature]],
                                          include.lowest = TRUE)
          split_bin_grad[[feature]] <- split(leaf$grad, split_bin_ref[[feature]])
          split_bin_hess[[feature]] <- split(leaf$hess, split_bin_ref[[feature]])
        }
      }
      else {
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