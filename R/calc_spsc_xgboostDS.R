
# TODO: What if data is too big to load into the environment all at once?
calc_spsc_xgboostDS <- function(data_name, spp_cand, lambda){
  
  data_sep <- eval(parse(text = paste0(data_name, "_sep")), 
                   envir = parent.frame())
  
  training_features <- data_sep[[1]][[1]]
  
  # TODO: wollen durch spp_cand gehen und dann jeweils score berechnen
  # Wie geht das effizient?
  
}