
#' Save the prediction through a tree.
#'
#' @param data_name Name of the data.
#' @param tree The trained tree which shall be saved.
#' @param amt_drops How many trees got dropped.
#'
#' @return The prediction through the newly trained tree.
#' @export
save_treeDS <- function(data_name, tree, amt_drops) {
  
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                       envir = parent.frame())
  data_by_row <- split(training_data, seq_len(nrow(training_data)))
  new_pred <- sapply(X = data_by_row, FUN = tree_evaluationDS, tree,
                     data_classes)
  if (amt_drops != 0) {
    new_pred <- new_pred / (amt_drops + 1)
  }
  
  return(new_pred)
}