
#' Save a sum of prediction through single trees.
#'
#' @param data_name Name of the data.
#' @param removed_trees Which trees got removed for training.
#' @param added_trees How many trees have been trained in total.
#'
#' @return The full predicition.
#' @export
update_full_treeDS <- function(data_name, removed_trees, added_trees) {
  
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                    envir = parent.frame())
  weight_update <- eval(parse(text = paste0(data_name, "_weight_update")),
                        envir = parent.frame())
  amt_drops <- length(removed_trees)
  for (number in removed_trees) {
    cur_tree <- eval(parse(text = paste0(data_name, "_tree_", number)),
                     envir = parent.frame())
    training_data$full_tree <- training_data$full_tree -
                               (1 / (amt_drops + 1)) * cur_tree
  }
  
  for (i in added_trees) {
    cur_tree <- eval(parse(text = paste0(data_name, "_tree_", i)),
                     envir = parent.frame())
    if (weight_update == "hessian") {
      training_data$full_tree <- training_data$full_tree + cur_tree
    } else if (weight_update == "average") {
      # TODO: Update if loss function with initial value != 0 exists
      training_data$full_tree <- (cur_tree + training_data$full_tree *
                                 (added_trees - 1)) / added_trees
    }
  }
  
  return(training_data)
}