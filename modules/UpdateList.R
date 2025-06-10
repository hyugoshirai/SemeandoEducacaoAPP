### 1. Function to update the reactive list ----
updateList <- function(layer, name, reactive_list) {
  # Get the current layers
  current_layer <- reactive_list()
  
  # Add or update the layer with the given name
  current_layer[[name]] <- layer
  
  # Update the reactive list
  reactive_list(current_layer)
  
  # Print for debugging (optional)
  print(paste("Updated list:", name))
  print(reactive_list())
}
