### 1. Create reactive values for the features table
### This code snippet creates a reactive value to store the features table data
features_table_data <- reactiveVal(data.frame(ID = character(), Name = character(), Feature = character(), Tipo = character(), stringsAsFactors = FALSE))

### 2. Function to update the features table
### This function updates the features table with the current features
updateFeaturesTable <- function() {
  features_df <- data.frame(
    ID = names(features_list()),
    Name = sapply(features_list(), function(x) x$name),
    Feature = sapply(features_list(), function(x) as.character(st_geometry_type(x$geometry))),
    Tipo = sapply(features_list(), function(x) x$Tipo),
    stringsAsFactors = FALSE
  )
  features_table_data(features_df)
}
