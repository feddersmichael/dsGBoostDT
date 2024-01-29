
#' Evaluate an output of a data-point under a tree.
#'
#' @param data_point The data-point to be evaluated.
#' @param tree The tree which maps the data.
#' @param data_classes Data class for all features.
#'
#' @return The output under the tree.
#' @export
tree_evaluationDS <- function(data_point, tree, data_classes){
  # TODO: write wrapper for data frames instead of data points
  # TODO: check if unique boost time or not
  if (!is.data.frame(tree)){
    stop("'tree' needs to be an object of type 'data frame'.")
  }
  
  if (!all(tree$Feature %in% colnames(data_point))){
    stop("Some features of 'tree' aren't available in 'data_point'.")
  }
  
  cur_split_point <- 1
  weight <- NULL
  
  while(is.null(weight)){
    
    cur_feat <- tree$feature[[cur_split_point]]
    cur_spl_val <- tree$split_value[[cur_split_point]]
    if (data_classes[[cur_feat]] == "numeric") {
      
      cont_NA <- tree$cont_NA[[cur_split_point]]
      if (is.na(data_point[[cur_feat]])) {
        if (cont_NA == 1) {
          if (tree$w_s_left[[cur_split_point]]){
            weight <- tree$w_s_left_value[[cur_split_point]]
          }
          else {
            cur_split_point <- tree$w_s_left_value[[cur_split_point]]
          }
        }
        else {
          if (tree$w_s_right[[cur_split_point]]){
            weight <- tree$w_s_right_value[[cur_split_point]]
          }
          else {
            cur_split_point <- tree$w_s_right_value[[cur_split_point]]
          }
        }
      }
      else if (data_point[[cur_feat]] > cur_spl_val){
        if (tree$w_s_right[[cur_split_point]]){
          weight <- tree$w_s_right_value[[cur_split_point]]
        }
        else {
          cur_split_point <- tree$w_s_right_value[[cur_split_point]]
        }
      }
      else {
        if (tree$w_s_left[[cur_split_point]]){
          weight <- tree$w_s_left_value[[cur_split_point]]
        }
        else {
          cur_split_point <- tree$w_s_left_value[[cur_split_point]]
        }
      }
    }
    else {
      if (as.numeric(data_point[[cur_feat]]) > cur_spl_val){
        if (tree$w_s_right[[cur_split_point]]){
          weight <- tree$w_s_right_value[[cur_split_point]]
        }
        else {
          cur_split_point <- tree$w_s_right_value[[cur_split_point]]
        }
      }
      else {
        if (tree$w_s_left[[cur_split_point]]){
          weight <- tree$w_s_left_value[[cur_split_point]]
        }
        else {
          cur_split_point <- tree$w_s_left_value[[cur_split_point]]
        }
      }
    }
  }
  
  return(weight)
}