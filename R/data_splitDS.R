
#' Reduce the data to the last added leaves
#'
#' @param data_name Name of the data.
#' @param training_features All the training data per features.
#' @param bounds_and_levels List of maximum and minimum value for numeric and
#' levels for factor features.
#' @param current_tree The currently trained tree.
#' @param data_classes Data class for all features.
#'
#' @return The two last added leafs of the tree.
data_splitDS <- function(data_name, training_features, bounds_and_levels,
                         current_tree, data_classes){
  # TODO: Is data_name_ID needed?
  
  splits <- nrow(current_tree)
  data_enum <- paste0(data_name, "_ID")
  
  if (splits == 0) {
    output <- list(training_features)
  }
  else if (splits == 1) {
    cur_feature <- current_tree$feature[1]
    cur_spv <- current_tree$split_value[1]
    
    if (data_classes[cur_feature] == "numeric") {
      breaks <- c(bounds_and_levels[[cur_feature]][1], cur_spv,
                  bounds_and_levels[[cur_feature]][2])
      cuts <- cut(training_features[[cur_feature]], breaks)
    }
    else {
      breaks <- c(1, cur_spv, length(bounds_and_levels[[cur_feature]]))
      cuts <- cut(as.numeric(training_features[[cur_feature]]), breaks)
    }
    
    data_split <- split(training_features[[data_enum]], cuts)
    output <- list(training_features[data_split[[1]], ],
                   training_features[data_split[[2]], ] )
  }
  else {
    for (i in 1:splits-1) {
      direction <- current_tree$par_dir[splits]
      par_spp <- current_tree$par_spp[splits]
      
      cur_feature <- current_tree$feature[par_spp]
      cur_spv <- current_tree$split_value[par_spp]
      
      if (direction){
        if (data_classes[cur_feature] == "numeric") {
          relev_rows <- training_features[[cur_feature]] <= cur_spv
        }
        else {
          relev_rows <- as.numeric(training_features[[cur_feature]]) <= cur_spv
        }
      }
      else {
        if (data_classes[cur_feature] == "numeric") {
          relev_rows <- training_features[[cur_feature]] > cur_spv
        }
        else {
          relev_rows <- as.numeric(training_features[[cur_feature]]) > cur_spv
        }
      }
      
      training_features <- training_features[relev_rows, ]
      
      current_split <- par_spp
    }
    
    cur_feature <- current_tree$feature[splits]
    cur_spv <- current_tree$split_value[splits]
    
    if (data_classes[cur_feature] == "numeric") {
      breaks <- c(bounds_and_levels[[cur_feature]][1], cur_spv,
                  bounds_and_levels[[cur_feature]][2])
      cuts <- cut(training_features[[cur_feature]], breaks)
    }
    else {
      breaks <- c(1, cur_spv, length(bounds_and_levels[[cur_feature]]))
      cuts <- cut(as.numeric(training_features[[cur_feature]]), breaks)
    }
    
    # TODO: Can this be done? alternative 1:nrow(training_features)
    data_split <- split(training_features, cuts)
    output <- list(data_split[[1]], data_split[[2]])
  }
  
  return(output)
}