### 1. Combining All sf Objects:
### This function combines all sf objects into a single sf object.
combineFeatures <- function() {
  features <- features_list()
  combined_sf <- do.call(rbind, features)
  return(combined_sf)
}