data_class_numericDS <- function(name){
  
  data <- eval(parse(text = name), envir = parent.frame())
  
  for (i in 1:ncol(data)){
    if (!class(data[, i] %in% c("numeric", "integer"))){
      return(FALSE)
    }
  }
  
  return(TRUE)
}