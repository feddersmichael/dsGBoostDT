
# TODO: We only need to add the value  under the last trained tree to the
#       prediction.
output_predDS <- function(boosted_tree){
  
  # We first check all the inputs for appropriate class
  if (!is.data.frame(boosted_tree)){
    stop("'boosted_tree' needs to be an object of type 'data frame'.")
  }
  
  
}