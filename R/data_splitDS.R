
#' data_splitDS
#'
#' @param training_features 
#' @param min_max 
#' @param current_tree 
#'
#' @return
#' @export
#'
#' @examples
data_splitDS <- function(training_features, min_max, current_tree){
  
  current_split <- nrow(current_tree)
  while (current_split != 1){
    direction <- current_tree$par_dir[current_split]
    par_spp <- current_tree$par_spp[current_split]
    
    cur_feature <- current_tree$Feature[par_spp]
    cur_spv <- current_tree$split_value[par_spp]
    
    if (direction){
      relev_rows <- training_features[[cur_feature]] <= cur_spv
      training_features <- training_features[relev_rows, ]
    }
    else {
      relev_rows <- training_features[[cur_feature]] > cur_spv
      training_features <- training_features[relev_rows, ]
    }
    
    current_split <- par_spp
  }
  
  current_split <- nrow(current_tree)
  cur_feature <- current_tree$Feature[current_split]
  cur_spv <- current_tree$split_value[current_split]
  
  breaks <- c(min_max[1, cur_feature], cur_spv, min_max[2, cur_feature])
  cuts <- cut(training_features[[cur_feature]], breaks)
  data_split <- split(training_features[[1]], cuts)
  
  data_1 <- training_features[data_split[[1]], ]
  data_2 <- training_features[data_split[[2]], ]
  return(list(list(data_1), list(data_2)))
}