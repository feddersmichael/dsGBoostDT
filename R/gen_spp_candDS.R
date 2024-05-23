
#' Title Create splitting-point candidates.
#'
#' @param data_name Name of the data.
#' @param cand_select By which method the candidates shall be selected.
#'
#' @return The splitting-point candidates.
#' @export
gen_spp_candDS <- function(data_name, cand_select = NULL) {
  
  if (is.null(cand_select)) {
    cand_select <- eval(parse(text = paste0(data_name, "_cand_select")),
                        envir = parent.frame())
  }
  
  comunication_round <- eval(parse(text = paste0(data_name, "_comunication_round")),
                             envir = parent.frame())
  bounds_and_levels <- eval(parse(text = paste0(data_name, "_bounds_and_levels")),
                            envir = parent.frame())
  data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                       envir = parent.frame())
  amt_spp <- eval(parse(text = paste0(data_name, "_amt_spp")),
                  envir = parent.frame())
  selected_feat <- eval(parse(text = paste0(data_name, "_selected_feat")),
                        envir = parent.frame())
  spp_cand <- eval(parse(text = paste0(data_name, "_spp_cand")),
                   envir = parent.frame())
  
  if (comunication_round > 1) {
    if (cand_select[["numeric"]] == "ithess") {
      ithess_stop <- eval(parse(text = paste0(data_name, "_ithess_stop")),
                          envir = parent.frame())
      if (comunication_round > (ithess_stop + 1)) {
        new_num_spp <- FALSE
      } else {
        new_num_spp <- TRUE
      }
    } else if (cand_select[["numeric"]] %in% c("uniform", "loguniform")) {
      new_num_spp <- FALSE
    } else {
      new_num_spp <- TRUE
    }
  } else {
    new_num_spp <- TRUE
    if (cand_select[["numeric"]] == "uniform") {
      ithess_control <- eval(parse(text = paste0(data_name, "_cand_select")),
                             envir = parent.frame())
      if (ithess_control[["numeric"]] == "ithess") {
        selected_feat <- names(data_classes)
      }
    }
  }
  
  # TODO: Use hessians calculated by last tree
  if (cand_select[["numeric"]] == "ithess" && new_num_spp) {
    training_data <- eval(parse(text = paste0(data_name, "_training")),
                          envir = parent.frame())
    hessians <- hessiansDS(data_name, training_data, bounds_and_levels,
                           data_classes, spp_cand, selected_feat)
  }
  
  for (feature in selected_feat) {
    if (data_classes[[feature]] == "numeric") {
      if (new_num_spp) {
        if (cand_select[["numeric"]] == "ithess") {
          add_par <- list(hessians = hessians[[feature]],
                          prev_spp_cand = spp_cand[[feature]])
        }
        spp_cand[[feature]] <- dsGBoostDTClient::ds.gen_numeric_spp_cand(bounds_and_levels[[feature]],
                                                                         amt_spp[[feature]],
                                                                         cand_select[["numeric"]], add_par)
      }
    } else {
      spp_cand[[feature]] <- dsGBoostDTClient::ds.gen_factor_spp_cand(length(bounds_and_levels[[feature]]),
                                                                      amt_spp[[feature]], cand_select[["factor"]])
    }
  }
  
  return(spp_cand)
}