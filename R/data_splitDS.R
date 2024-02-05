
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
  } else if (splits == 1) {
    cur_feature <- current_tree$feature[1]
    cur_spv <- current_tree$split_value[1]
    cont_NA <- current_tree$cont_NA[1]

    if (data_classes[cur_feature] == "numeric") {
      breaks <- c(bounds_and_levels[[cur_feature]][1], cur_spv,
                  bounds_and_levels[[cur_feature]][2])
      cuts <- cut(training_data[[cur_feature]], breaks, include.lowest = TRUE)
    } else {
      breaks <- c(1, cur_spv, length(bounds_and_levels[[cur_feature]]))
      cuts <- cut(as.numeric(training_data[[cur_feature]]), breaks,
                  include.lowest = TRUE)
    }

    if (cont_NA != 0) {
      cuts <- addNA(cuts)
    }

    data_split <- split(training_data, cuts)

    if (cont_NA == 1) {
      data_split[[1]] <- rbind(data_split[[1]], data_split[[3]])
      data_split[[3]] <- NULL
    } else if (cont_NA == 2) {
      data_split[[2]] <- rbind(data_split[[2]], data_split[[3]])
      data_split[[3]] <- NULL
    }

    output <- list(data_split[[1]], data_split[[2]])
  } else {

    current_split <- splits

    while (current_split != 1) {
      direction <- current_tree$par_dir[current_split]
      par_spp <- current_tree$par_spp[current_split]

      cur_feature <- current_tree$feature[par_spp]
      cur_spv <- current_tree$split_value[par_spp]
      cont_NA <- current_tree$cont_NA[par_spp]

      if (direction) {
        if (data_classes[cur_feature] == "numeric") {
          if (cont_NA == 1) {
            relev_rows <- (training_data[[cur_feature]] <= cur_spv |
                             is.na(training_data[[cur_feature]]))
          } else {
            relev_rows <- training_data[[cur_feature]] <= cur_spv
          }
        } else {
          relev_rows <- as.numeric(training_data[[cur_feature]]) <= cur_spv
        }
      } else {
        if (data_classes[cur_feature] == "numeric") {
          if (cont_NA == 2) {
            relev_rows <- (training_data[[cur_feature]] > cur_spv |
                             is.na(training_data[[cur_feature]]))
          } else {
            relev_rows <- training_data[[cur_feature]] > cur_spv
          }
        } else {
          relev_rows <- as.numeric(training_data[[cur_feature]]) > cur_spv
        }
      }

      training_data <- training_data[relev_rows, ]

      current_split <- par_spp
    }

    cur_feature <- current_tree$feature[splits]
    cur_spv <- current_tree$split_value[splits]
    cont_NA <- current_tree$cont_NA[splits]

    if (data_classes[cur_feature] == "numeric") {
      breaks <- c(bounds_and_levels[[cur_feature]][1], cur_spv,
                  bounds_and_levels[[cur_feature]][2])
      cuts <- cut(training_data[[cur_feature]], breaks, include.lowest = TRUE)
    } else {
      breaks <- c(1, cur_spv, length(bounds_and_levels[[cur_feature]]))
      cuts <- cut(as.numeric(training_data[[cur_feature]]), breaks,
                  include.lowest = TRUE)
    }

    if (cont_NA != 0) {
      cuts <- addNA(cuts)
    }

    data_split <- split(training_data, cuts)

    if (cont_NA == 1) {
      data_split[[1]] <- rbind(data_split[[1]], data_split[[3]])
      data_split[[3]] <- NULL
    } else if (cont_NA == 2) {
      data_split[[2]] <- rbind(data_split[[2]], data_split[[3]])
      data_split[[3]] <- NULL
    }

    output <- list(data_split[[1]], data_split[[2]])
  }

  return(output)
}
