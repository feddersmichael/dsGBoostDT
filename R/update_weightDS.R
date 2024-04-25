
#' Update weights
#'
#' @param data_name The name under which the data is saved on the server.
#' @param current_tree The fully chosen tree which misses its weights.
#' @param max_splits The maximum amount of splits in the trained tree.
#'
#' @return The weights for each leaf.
#' @export
update_weightDS <- function(data_name, current_tree, max_splits) {
  
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  bounds_and_levels <- eval(parse(text = paste0(data_name, "_bounds_and_levels")),
                            envir = parent.frame())
  data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                       envir = parent.frame())
  weight_update <- eval(parse(text = paste0(data_name, "_weight_update")),
                        envir = parent.frame())
  output_var <- eval(parse(text = paste0(data_name, "_output_var")),
                     envir = parent.frame())
  
  # TODO: replace max_splits
  leaves <- data_splitDS(training_data, bounds_and_levels, current_tree,
                         data_classes, "split_row", max_splits)
  
  leaf_weights <- list()
  if (weight_update == "average") {
    for (i in 1:length(leaves)) {
      output <- leaves[[i]][[output_var]]
      weight_upd <- list()
      weight_upd[["output_sum"]] <- sum(output)
      weight_upd[["amt_data"]] <- length(output)
      leaf_weights[[i]] <- weight_upd
    }
  } else if (weight_update == "hessian") {
    for (i in 1:length(leaves)) {
      weight_upd <- list()
      weight_upd[["gradient"]] <- sum(leaves[[i]]$grad)
      weight_upd[["hessian"]] <- sum(leaves[[i]]$hess)
      leaf_weights[[i]] <- weight_upd
    }
  }
  
  return(leaf_weights)
}