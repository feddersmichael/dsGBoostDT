
#' Retrieve the hessian bins
#'
#' @param data_name The name of the data.
#'
#' @return The hessian bins.
#' @export
hessiansDS <- function(data_name, spp_cand = NULL) {
  
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  bounds_and_levels <- eval(parse(text = paste0(data_name, "_bounds_and_levels")),
                            envir = parent.frame())
  data_classes <- eval(parse(text = paste0(data_name, "_data_classes")),
                    envir = parent.frame())
  if (is.null(spp_cand)) {
    spp_cand <- eval(parse(text = paste0(data_name, "_spp_cand")),
                     envir = parent.frame())
  } else {
    spp_cand <- spp_cand
  }
  selected_feat <- eval(parse(text = paste0(data_name, "_selected_feat")),
                        envir = parent.frame())
  
  if (is.null(selected_feat)) {
    features <- names(data_classes)
  } else {
    features <- selected_feat
  }
  
  numerics <- sapply(features, function(name){return(data_classes[[name]] == "numeric")})
  features <- features[numerics]
  # We also prepare our breaks to cut the data into bins
  breaks <- list()
  for (feature in features) {
    breaks[[feature]] <- c(bounds_and_levels[[feature]][1],
                           spp_cand[[feature]],
                           bounds_and_levels[[feature]][2])
  }
  leaf <- training_data
  split_bin_ref <- list()
  split_bin_hess <- list()
  for (feature in features) {
    split_bin_ref[[feature]] <- addNA(cut(leaf[[feature]],
                                          breaks[[feature]],
                                          include.lowest = TRUE))
    NA_index <- length(levels(split_bin_ref[[feature]]))
    split_bin_hess[[feature]] <- split(leaf$hess, split_bin_ref[[feature]])
    names(split_bin_hess[[feature]])[NA_index] <- "NA"
    split_bin_hess[[feature]] <- lapply(split_bin_hess[[feature]], sum)
  }
  
  return(split_bin_hess)
}