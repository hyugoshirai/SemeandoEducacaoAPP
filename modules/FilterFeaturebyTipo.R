# Define the filtering function
FilterFeaturebyTipo <- function(features_list, tipo_value) {
  filtered_list <- lapply(features_list, function(feature) {
    feature_filtered <- feature %>% filter(Tipo == tipo_value)
    
    # Check if any rows are left after filtering
    if (nrow(feature_filtered) > 0) {
      return(feature_filtered)
    } else {
      return(NULL)
    }
  })
  
  # Remove NULL elements from the list
  filtered_list <- filtered_list[!sapply(filtered_list, is.null)]
  
  return(filtered_list)
}