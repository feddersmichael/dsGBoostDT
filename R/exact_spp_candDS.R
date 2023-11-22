
# data = name of data, string
exact_spp_candDS <- function(data_name){
  
  # We convert the string into an object
  training_data <- eval(parse(text = data_name), envir = parent.frame())
  
  # should probably just be saved from the start
  column_names <- names(training_data)
  
  
  
}