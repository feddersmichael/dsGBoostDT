
#' Save a sum of prediction through single trees.
#'
#' @param data_name Name of the data.
#' @param removed_trees Which trees got removed for training.
#' @param last_tree_nmb How many trees have been trained in total.
#'
#' @return The full predicition.
#' @export
update_full_treeDS <- function(data_name, removed_trees, last_tree_nmb) {
  
  training_data <- NULL
  if (last_tree_nmb != 0) {
    training_data <- eval(parse(text = paste0(data_name, "_training")),
                      envir = parent.frame())
    full_tree <- training_data$full_tree
    amt_drops <- length(removed_trees)
    for (number in removed_trees) {
      cur_tree <- eval(parse(text = paste0(data_name, "_tree_", number)),
                       envir = parent.frame())
      full_tree <- full_tree - (1 / (amt_drops + 1)) * cur_tree
    }
    last_tree <- eval(parse(text = paste0(data_name, "_tree_", last_tree_nmb)),
                      envir = parent.frame())
    full_tree <- full_tree + last_tree
    training_data$full_tree <- full_tree
  }
  return(training_data)
}