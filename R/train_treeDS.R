
#' Train a remote tree.
#'
#' @param data_name Name of the data.
#'
#' @return The trained tree.
#' @export
train_treeDS <- function(data_name) {
  
  # We first check all the inputs for appropriate class
  if (!is.character(data_name)) {
    stop("'data_name' needs to have data type 'character'.")
  }
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  max_splits <- eval(parse(text = paste0(data_name, "_max_splits")),
                      envir = parent.frame())
  bounds_and_levels <- eval(parse(text = paste0(data_name, "_bounds_and_levels")),
                            envir = parent.frame())
  selected_feat <- eval(parse(text = paste0(data_name, "_selected_feat")),
                        envir = parent.frame())
  spp_cand <- eval(parse(text = paste0(data_name, "_spp_cand")),
                   envir = parent.frame())
  data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                       envir = parent.frame())
  reg_par <- eval(parse(text = paste0(data_name, "_reg_par")),
                  envir = parent.frame())
  
  current_tree <- data.frame(feature = character(), split_value = numeric(),
                             cont_NA = numeric(), w_s_left = logical(),
                             w_s_left_value = numeric(), w_s_right = logical(),
                             w_s_right_value = numeric(), par_spp = numeric(),
                             par_dir = logical())
  split_scores_left <- data.frame(sp_sc = numeric(), feature = character(),
                                  split_val = numeric(), cont_NA = numeric(),
                                  weight_l = numeric(), weight_r = numeric())
  
  split_scores_right <- data.frame(sp_sc = numeric(), feature = character(),
                                   split_val = numeric(), cont_NA = numeric(),
                                   weight_l = numeric(), weight_r = numeric())
  
  leaves_list <- NULL
  for (i in 1:max_splits) {
    
    if (i == 1) {
      leaves_list <- list(training_data)
    } else {
      leaves_list <- saveleafDS(data_name, current_tree, leaves_list,
                                bounds_and_levels, data_classes)
    }
    histograms_per_leaf <- split_binsDS(data_name, leaves_list,
                                        bounds_and_levels, data_classes,
                                        spp_cand, selected_feat)
    amt_leaves <- length(histograms_per_leaf)
    
    for (j in 1:amt_leaves) {
      cont_NA <- logical()
      for (feature in selected_feat) {
        categories <- histograms_per_leaf[[j]][["grad"]][[feature]]
        if ("NA" %in% names(categories) && data_classes[[feature]] == "numeric" &&
            (categories["NA"] != 0)) {
          cont_NA[[feature]] <- TRUE
        } else {
          cont_NA[[feature]] <- FALSE
        }
      }
      histograms_per_leaf[[j]][["cont_NA"]] <- cont_NA
    }
    
    best_split <- dsGBoostDTClient::ds.select_split(histograms_per_leaf,
                                                    spp_cand, data_classes,
                                                    reg_par)
    
    if (i == 1) {
      next_split <- list(best_split$feature[[1]], best_split$split_val[[1]],
                         best_split$cont_NA[[1]], TRUE, best_split$weight_l[[1]],
                         TRUE, best_split$weight_r[[1]], 0, TRUE)
      current_tree[1, ] <- next_split
    } else {
      # TODO: Fix rownames for copying rows into df
      split_scores_left[i - 1, ] <- best_split[1, ]
      split_scores_right[i - 1, ] <- best_split[2, ]
      
      max_l_index <- which.max(split_scores_left$sp_sc)
      max_r_index <- which.max(split_scores_right$sp_sc)
      
      max_l <- split_scores_left$sp_sc[[max_l_index]]
      max_r <- split_scores_right$sp_sc[[max_r_index]]
      
      if (max_l > 0 || max_r > 0) {
        if (max_l > max_r) {
          next_split <- split_scores_left[max_l_index, ]
          current_tree[i, ] <- list(next_split$feature[[1]],
                                    next_split$split_val[[1]],
                                    next_split$cont_NA[[1]], TRUE,
                                    next_split$weight_l[[1]], TRUE,
                                    next_split$weight_r[[1]], max_l_index,
                                    TRUE)
          current_tree$w_s_left[[max_l_index]] <- FALSE
          current_tree$w_s_left_value[[max_l_index]] <- i
          split_scores_left$sp_sc[[max_l_index]] <- 0
        } else {
          next_split <- split_scores_right[max_r_index, ]
          current_tree[i, ] <- list(next_split$feature[[1]],
                                    next_split$split_val[[1]],
                                    next_split$cont_NA[[1]], TRUE,
                                    next_split$weight_l[[1]], TRUE,
                                    next_split$weight_r[[1]], max_r_index,
                                    FALSE)
          current_tree$w_s_right[[max_r_index]] <- FALSE
          current_tree$w_s_right_value[[max_r_index]] <- i
          split_scores_right$sp_sc[[max_r_index]] <- 0
        }
      } else {
        break
      }
    }
    
  }
  
  return(current_tree)
}