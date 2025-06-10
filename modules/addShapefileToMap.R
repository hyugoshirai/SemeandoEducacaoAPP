### 1.  Function to add the shapefile to the map
### This function adds a shapefile to the map and updates the layers control
### shapefile_data: The shapefile data to be added
### new_layer_name: The name of the new layer to be added
### current_layers: The current layers on the map

addShapefileToMap <- function(shapefile_data, new_layer_name) {
  # Get the names of the current layers and add the new shapefile name to the list
  current_layers <- getCurrentLayerNames()
  
  if (!is.null(shapefile_data)) {
    shapefile_data <- st_transform(shapefile_data, crs = 4326)
    
    # Determine the geometry type of the shapefile
    geometry_type <- unique(st_geometry_type(shapefile_data))
    if (geometry_type == "POLYGON") {
      addShp <- leafletProxy("map") %>%
        addPolygons(data = shapefile_data, fillColor = "transparent", color = "blue", weight = 2, group = new_layer_name)
    } else if (geometry_type == "LINESTRING") {
      addShp <- leafletProxy("map") %>%
        addPolylines(data = shapefile_data, color = "blue", weight = 2, group = new_layer_name)
    } else if (geometry_type == "POINT") {
      addShp <- leafletProxy("map") %>%
        addMarkers(data = shapefile_data, group = new_layer_name)
    }
    addShp |> 
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"), # Always include base groups
        overlayGroups = c(custom_control,current_layers),  # Ensure all layers, including new ones, are listed
        options = layersControlOptions(collapsed = TRUE)
      )
    showNotification(paste(new_layer_name, "added to the map."), type = "message")
  } else {
    showNotification("Please upload a shapefile first.", type = "warning")
  }
  updateShapefileList (shapefile_data, new_layer_name)
}