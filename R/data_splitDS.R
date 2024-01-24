
#' Reduce the data to the last added leaves
#'
#' @param training_data All the training data per features.
#' @param bounds_and_levels List of maximum and minimum value for numeric and
#' levels for factor features.
#' @param current_tree The currently trained tree.
#' @param data_classes Data class for all features.
#'
#' @return The two last added leafs of the tree.
#' @export
data_splitDS <- function(training_data, bounds_and_levels, current_tree,
                         data_classes) {

  splits <- nrow(current_tree)

  if (splits == 0) {
    output <- list(training_data)
  }
  else if (splits == 1) {
    cur_feature <- current_tree$feature[1]
    cur_spv <- current_tree$split_value[1]

    if (data_classes[cur_feature] == "numeric") {
      breaks <- c(bounds_and_levels[[cur_feature]][1], cur_spv,
                  bounds_and_levels[[cur_feature]][2])
      cuts <- cut(training_data[[cur_feature]], breaks, include.lowest = TRUE)
    }
    else {
      breaks <- c(1, cur_spv, length(bounds_and_levels[[cur_feature]]))
      cuts <- cut(as.numeric(training_data[[cur_feature]]), breaks,
                  include.lowest = TRUE)
    }

    data_split <- split(training_data, cuts)
    output <- list(data_split[[1]], data_split[[2]])
  }
  else {
    for (i in 1:(splits - 1)) {
      direction <- current_tree$par_dir[splits]
      par_spp <- current_tree$par_spp[splits]

      cur_feature <- current_tree$feature[par_spp]
      cur_spv <- current_tree$split_value[par_spp]

      if (direction) {
        if (data_classes[cur_feature] == "numeric") {
          relev_rows <- training_data[[cur_feature]] <= cur_spv
        }
        else {
          relev_rows <- as.numeric(training_data[[cur_feature]]) <= cur_spv
        }
      }
      else {
        if (data_classes[cur_feature] == "numeric") {
          relev_rows <- training_data[[cur_feature]] > cur_spv
        }
        else {
          relev_rows <- as.numeric(training_data[[cur_feature]]) > cur_spv
        }
      }

      training_data <- training_data[relev_rows, ]

      current_split <- par_spp
    }

    cur_feature <- current_tree$feature[splits]
    cur_spv <- current_tree$split_value[splits]

    if (data_classes[cur_feature] == "numeric") {
      breaks <- c(bounds_and_levels[[cur_feature]][1], cur_spv,
                  bounds_and_levels[[cur_feature]][2])
      cuts <- cut(training_data[[cur_feature]], breaks, include.lowest = TRUE)
    }
    else {
      breaks <- c(1, cur_spv, length(bounds_and_levels[[cur_feature]]))
      cuts <- cut(as.numeric(training_data[[cur_feature]]), breaks,
                  include.lowest = TRUE)
    }

    # TODO: Can this be done? alternative 1:nrow(training_data)
    data_split <- split(training_data, cuts)
    output <- list(data_split[[1]], data_split[[2]])
  }

  return(output)
}