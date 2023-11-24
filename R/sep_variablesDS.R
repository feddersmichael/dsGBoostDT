
# TODO: error checking for mode
sep_variablesDS <- function(data_name, mode){
  
  data_set <- eval(parse(text = data_name), envir = parent.frame())
  column_names <- eval(parse(text = 'column_names'), envir = parent.frame())
  
  no_var <- length(column_names)
  
  if (mode == "features"){
    return(data_set[,1:no_var-1])
  }
  
  if (mode == "output"){
    return(data_set[column_names[no_var]])
  }
  
}