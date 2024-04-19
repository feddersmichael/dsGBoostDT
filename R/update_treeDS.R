
#' Update weight of the prediction of a tree.
#'
#' @param data_name Name of the data.
#' @param tree_number Which tree shall be updated.
#' @param amt_drops How many trees were dropped for the training.
#'
#' @return The adapted tree.
#' @export
update_treeDS <- function(data_name, tree_number, amt_drops) {
  
  tree <- eval(parse(text = paste0(data_name, "_tree_", tree_number)),
                   envir = parent.frame())
  tree <- (tree * amt_drops)  / (amt_drops + 1)
  
  return(tree)
}