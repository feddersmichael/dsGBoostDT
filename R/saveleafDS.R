
#' Save the split leaves on the server.
#'
#' @param data_name The name of the data.
#' @param current_tree The tree which gets currently trained.
#' @param leaves_list The list of all leaves, input if training on server.
#' @param bounds_and_levels The maximum and minimum values for numeric features
#' and levels for factor features.
#' @param data_classes Data class for all features.
#'
#' @return The newly created leaves.
#' @export
saveleafDS <- function(data_name, current_tree, leaves_list = NULL,
                       bounds_and_levels = NULL, data_classes = NULL) {
  
  amt_splits <- nrow(current_tree)
  if (amt_splits == 0) {
    training_data <- eval(parse(text = paste0(data_name, "_training")),
                          envir = parent.frame())
    return(list(training_data))
  } else if (amt_splits == 1) {
    if (is.null(leaves_list)) {
      leaves_list <- eval(parse(text = paste0(data_name, "_leaves")),
                          envir = parent.frame())
    }
    parent_leaf <- leaves_list[[1]]
  } else {
    if (is.null(leaves_list)) {
      leaves_list <- eval(parse(text = paste0(data_name, "_leaves")),
                          envir = parent.frame())
    }
    if (current_tree$par_dir[[amt_splits]]) {
      parent_leaf <- leaves_list[[2 * current_tree$par_spp[[amt_splits]] - 1]]
    } else {
      parent_leaf <- leaves_list[[2 * current_tree$par_spp[[amt_splits]]]]
    }
  }
  
  if (is.null(bounds_and_levels)) {
    bounds_and_levels <- eval(parse(text = paste0(data_name, "_bounds_and_levels")),
                              envir = parent.frame())
  }
  
  if (is.null(data_classes)) {
    data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                         envir = parent.frame())
  }
  
  leaves <- data_splitDS(parent_leaf, bounds_and_levels,
                         current_tree[amt_splits, ], data_classes, "leaf", 1)
  
  leaves_list[[2 * amt_splits - 1]] <- leaves[[1]]
  leaves_list[[2 * amt_splits]] <- leaves[[2]]
  
  return(leaves_list)
}