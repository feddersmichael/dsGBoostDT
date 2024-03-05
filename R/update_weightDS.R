
update_weightDS <- function(data_name, current_tree, max_splits, weight_update,
                            loss_function, output_var) {
  
  training_data <- eval(parse(text = paste0(data_name, "_training")),
                        envir = parent.frame())
  
  # TODO
  leaves <- data_splitDS(training_data, bounds_and_levels, current_tree,
                         data_classes)
  
  weights <- numeric()
  if (weight_update == "average") {
    for (leaf in leaves) {
      output <- training_data[[output_var]]
      weights <- c(weights, sum(output) / length(output))
    }
  } else if (weight_update == "hessian") {
    for (leaf in leaves) {
      
    }
  }
  
}