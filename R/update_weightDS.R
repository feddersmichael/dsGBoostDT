
#' Update weights
#'
#' @param data_name The name under which the data is saved on the server.
#' @param current_tree The fully chosen tree which misses its weights.
#' @param bounds_and_levels The maximum and minimum values for numeric features
#' and levels for factor features. 
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param data_classes List of data class per column.
#' @param weight_update Through which method we choose the weights for our tree.
#' @param loss_function The type of loss function under which we optimise the
#' tree.
#' @param output_var Name of the output variable.
#'
#' @return The weights for each leaf.
#' @export
update_weightDS <- function(data_name, current_tree, bounds_and_levels,
                            max_splits, data_classes, weight_update,
                            loss_function, output_var) {
  
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  
  leaves <- data_splitDS(training_data, bounds_and_levels, current_tree,
                         data_classes, "split_row", max_splits)
  
  leaf_weights <- list()
  if (weight_update == "average") {
    for (i in 1:length(leaves)) {
      output <- leaves[[i]][[output_var]]
      leaf_weights[[i]][["output_sum"]] <- sum(output)
      leaf_weights[[i]][["amt_data"]] <- length(output)
    }
  } else if (weight_update == "hessian") {
    for (i in length(leaves)) {
      leaf_weights[[i]][["gradient"]] <- sum(leaves[[i]]$grad)
      leaf_weights[[i]][["hessian"]] <- sum(leaves[[i]]$hess)
    }
  }
  
  return(leaf_weights)
}