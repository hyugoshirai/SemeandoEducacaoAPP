### 1. Handle new feature draw
### This function handles the new feature drawn on the map
### input: input - the input object from the Shiny app
### feature - the feature drawn on the map
### proj_crs - the projected coordinate reference system (CRS) for area calculations
### geo_crs - the geographic CRS for displaying the feature
handleNewFeature <- function(input, feature, proj_crs = 32723, geo_crs = 4326) {
  feature_type <- feature$properties$feature_type  # Get the type of the geometry
  if (feature_type == "rectangle" | feature_type == "polygon") {
    coords <- feature$geometry$coordinates[[1]] # Extract the coordinates
    coords_matrix <- do.call(rbind, lapply(coords, unlist)) # Convert coordinates to matrix
    
    feature_sf <- st_sfc(st_polygon(list(coords_matrix))) # Create the sf object
  } else if (feature_type == "marker") {
    coords <- unlist(feature$geometry$coordinates) # Extract and unlist the coordinates for point
    print(coords)
    feature_sf <- st_sfc(st_point(coords)) # Create the sf object for point
  } else if (feature_type == "circle") {
    center <- unlist(feature$geometry$coordinates)  # Extract and unlist the center coordinates
    radius <- feature$properties$radius  # Extract the radius in meters
    # Create a point geometry in WGS 84 (longitude/latitude)
    point <- st_sfc(st_point(center), crs = geo_crs)
    # Transform the point to a projected CRS that uses meters (e.g., UTM Zone 23S)
    point_projected <- st_transform(point, crs = proj_crs)
    # Apply the buffer in the projected CRS to create the circle as a polygon
    circle_polygon_projected <- st_buffer(point_projected, dist = radius)
    # Transform the circle polygon back to WGS 84
    circle_polygon <- st_transform(circle_polygon_projected, crs = geo_crs)
    circle_polygon <- st_set_crs(circle_polygon_projected, NA)
    feature_sf <- st_sfc(circle_polygon)  # Create the sf object
    # Optionally update the feature type to "POLYGON"
    feature_type <- "POLYGON"
  }
  else if (feature_type == "polyline") {
    coords <- feature$geometry$coordinates
    print (coords)
    coords_matrix <- do.call(rbind, lapply(coords, function(coord) unlist(coord)))
    print (coords_matrix)
    
    feature_sf <- st_sfc(st_linestring(coords_matrix)) # Create the sf object for polyline
  }
  
  feature_id <- feature$properties$`_leaflet_id`# Extract the feature ID
  feature_name <- paste("Feature", feature_id) # Initialize a feature name

  # Create the sf object with the Mapping_Input as an additional column
  feature_sf <- st_sf(id = feature_id, name = feature_name, geometry = feature_sf, Tipo = input$MappingInput)
  
  current_features <- features_list() # Retrieve the current features list
  current_features[[as.character(feature_id)]] <- feature_sf # Add the new feature to the list
  features_list(current_features) # Save the updated list back to the reactive value
  
  updateFeaturesTable() # Update the data table
  updateFeatureLabels() # Update feature labels
  
  showNotification(paste("Feature", feature_id, "has been added"), duration = 5, type = "message") # Inform the user that the feature has been added
}

### 2. Handle feature edit
### This function handles the feature edit event
### input: input - the input object from the Shiny app
### edited_features - the edited feature object
handleFeatureEdit <- function(input, edited_features) {
  # Extract the feature ID and coordinates
  edited_feature_id <- edited_features$features[[1]]$properties$`_leaflet_id`
  feature_type <- edited_features$features[[1]]$geometry$type
  edited_coords <- edited_features$features[[1]]$geometry$coordinates
  
  # Initialize the sf object based on the feature type
  if (feature_type == "Point") {
    coords <- unlist(edited_coords)  # Flatten the list if necessary
    edited_feature_sf <- st_sfc(st_point(coords))
    
  } else if (feature_type == "LineString") {
    # Convert the coordinates to a matrix
    edited_coords_matrix <- do.call(rbind, lapply(edited_coords, unlist))
    edited_feature_sf <- st_sfc(st_linestring(edited_coords_matrix))
    
  } else if (feature_type == "Polygon") {
    edited_coords <- edited_features$features[[1]]$geometry$coordinates[[1]] # Extract the coordinates
    edited_coords_matrix <- do.call(rbind, lapply(edited_coords, unlist)) # Convert coordinates to matrix
    edited_feature_sf <- st_sfc(st_polygon(list(edited_coords_matrix))) # Create the sf object
  } else {
    showNotification("Unsupported feature type.", type = "error")
    return()
  }
  
  # Retrieve and update the current features list
  # current_features <- features_list()
  # current_features[[as.character(edited_feature_id)]]$geometry <- edited_feature_sf
  # features_list(current_features)
  updateList(edited_feature_sf, as.character(edited_feature_id), features_list) # Update the feature in the list
  
  # Update the data table and feature labels
  updateFeaturesTable()
  updateFeatureLabels()
  
  # Notify the user about the update
  showNotification(paste("Feature", edited_feature_id, "has been updated."), duration = 5, type = "message")
}


### 3. Handle cell edit in the data table
### This function handles the name cell edit event in the data table
### input: input - the input object from the Shiny app
### new_data - the new data object containing the edited cell information
handleCellEdit <- function(input, new_data) {
  row <- new_data$row # Extract the row number
  col <- new_data$col # Extract the column number
  new_value <- new_data$value # Extract the new value
  
  current_features <- features_list()  # Retrieve the current features list
  feature_id <- names(current_features)[row]  # Extract the feature ID
  current_features[[feature_id]]$name <- new_value  # Update the feature name
  features_list(current_features)  # Save the updated list back to the reactive value
  
  updateFeaturesTable()  # Update the data table
  updateFeatureLabels()  # Update feature labels
  
  # Inform the user that the feature name has been updated
  showNotification(paste("Feature name has been updated to:", new_value), duration = 5, type = "message")
}

### 4. Handle feature deletion
### This function handles the feature deletion event
### input: input - the input object from the Shiny app
### deleted_features - the deleted feature object
handleFeatureDeletion <- function(input, deleted_features) {
  feature_ids <- sapply(deleted_features$features, function(f) f$properties$`_leaflet_id`) # Extract the feature IDs
  
  current_features <- features_list() # Retrieve the current features list
  current_features <- current_features[!names(current_features) %in% feature_ids] # Remove the deleted features
  features_list(current_features) # Save the updated list back to the reactive value
  
  updateFeaturesTable() # Update the data table
  updateFeatureLabels() # Update feature labels
  
  showNotification(paste("Features", paste(feature_ids, collapse = ", "), "have been deleted."), duration = 5, type = "message") # Inform the user that the features have been deleted
}