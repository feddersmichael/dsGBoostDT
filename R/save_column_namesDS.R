
# TODO: need check for df and string
save_column_namesDS <- function(data_name){
  
  current_data <- eval(parse(text = data_name), envir = parent.frame())
  output <- names(current_data)
  return(output)
}