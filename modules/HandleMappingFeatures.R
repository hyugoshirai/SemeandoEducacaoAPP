### 1. Handle new feature draw
### This function handles the new feature drawn on the map
### input: input - the input object from the Shiny app
### feature - the feature drawn on the map
### proj_crs - the projected coordinate reference system (CRS) for area calculations
### geo_crs - the geographic CRS for displaying the feature

# Function to get color based on type
get_tipo_color <- function(tipo) {
  col <- color_mapping$color[match(tipo, color_mapping$label)]
  ifelse(is.na(col), "#2c3e50", col)
}


# Helper to extract the right feature id from the edit/delete event
get_event_feature_id <- function(feat_event, existing_keys) {
  props <- feat_event$features[[1]]$properties
  # Gather candidates in priority order
  cand <- c(props$layerId, props$id, props$`_leaflet_id`)
  cand <- as.character(cand[!vapply(cand, is.null, logical(1))])
  # Pick the first that exists in your features_list keys; fallback to last candidate
  hit <- cand[cand %in% existing_keys]
  fid <- if (length(hit)) hit[1] else cand[length(cand)]
  message("handle: candidates=", paste(cand, collapse = ", "),
          " | chosen id=", fid,
          " | in_keys=", fid %in% existing_keys)
  fid
}

### 1. Handle new feature draw
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
  # feature_sf <- st_sf(id = feature_id, name = feature_name, geometry = feature_sf, Tipo = input$MappingInput)
  feature_sf <- st_sf(
    id = feature_id,
    name = feature_name,
    Tipo = input$MappingInput,
    color = get_tipo_color(input$MappingInput),
    geometry = feature_sf
  )
  
  current_features <- features_list() # Retrieve the current features list
  current_features[[as.character(feature_id)]] <- feature_sf # Add the new feature to the list
  features_list(current_features) # Save the updated list back to the reactive value
  
  updateFeaturesTable() # Update the data table
  updateFeatureLabels() # Update feature labels
  
  showNotification(paste("Feição", feature_id, "foi criada"), duration = 5, type = "message") # Inform the user that the feature has been added
}




### 2. Handle feature edit
### This function handles the feature edit event
### input: input - the input object from the Shiny app
### edited_features - the edited feature object
handleFeatureEdit <- function(input, edited_features, geo_crs = 4326) {
  # Build sfc with CRS
  feature_type <- edited_features$features[[1]]$geometry$type
  edited_coords <- edited_features$features[[1]]$geometry$coordinates
  
  if (feature_type == "Point") {
    coords <- unlist(edited_coords)
    edited_sfc <- sf::st_sfc(sf::st_point(coords), crs = geo_crs)
  } else if (feature_type == "LineString") {
    edited_coords_matrix <- do.call(rbind, lapply(edited_coords, unlist))
    edited_sfc <- sf::st_sfc(sf::st_linestring(edited_coords_matrix), crs = geo_crs)
  } else if (feature_type == "Polygon") {
    edited_coords <- edited_coords[[1]]
    edited_coords_matrix <- do.call(rbind, lapply(edited_coords, unlist))
    edited_sfc <- sf::st_sfc(sf::st_polygon(list(edited_coords_matrix)), crs = geo_crs)
  } else {
    showNotification("Unsupported feature type.", type = "error")
    return()
  }
  
  # Resolve the correct feature id (works for both drawn and loaded)
  current <- features_list()
  keys <- names(current)
  fid <- get_event_feature_id(edited_features, keys)
  
  if (!is.null(current[[fid]]) && inherits(current[[fid]], "sf")) {
    feat <- current[[fid]]
    sf::st_geometry(feat) <- edited_sfc
    current[[fid]] <- feat
    features_list(current)
    message("Edited feature updated | id=", fid, " | type=", feature_type)
  } else {
    # If we still can't find it, create/update minimally to avoid losing the change
    message("Edited id not found in features_list; creating minimal record | id=", fid)
    feat <- sf::st_sf(
      id = fid,
      name = if (!is.null(current[[fid]]) && "name" %in% names(current[[fid]])) current[[fid]]$name else paste("Feature", fid),
      Tipo = if (!is.null(current[[fid]]) && "Tipo" %in% names(current[[fid]])) current[[fid]]$Tipo else NA_character_,
      color = if (!is.null(current[[fid]]) && "color" %in% names(current[[fid]])) current[[fid]]$color else "#2c3e50",
      geometry = edited_sfc
    )
    current[[fid]] <- feat
    features_list(current)
  }
  
  updateFeaturesTable()
  updateFeatureLabels()
  showNotification(paste("Feição", fid, "foi atualizada."), duration = 5, type = "message")
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
  showNotification(paste("O nome da feição foi atualizado para:", new_value), duration = 5, type = "message")
}

### 4. Handle feature deletion
### This function handles the feature deletion event
### input: input - the input object from the Shiny app
### deleted_features - the deleted feature object
handleFeatureDeletion <- function(input, deleted_features) {
  current <- features_list()
  keys <- names(current)
  
  feature_ids <- vapply(deleted_features$features, function(f) {
    props <- f$properties
    cand <- c(props$layerId, props$id, props$`_leaflet_id`)
    cand <- as.character(cand[!vapply(cand, is.null, logical(1))])
    hit <- cand[cand %in% keys]
    if (length(hit)) hit[1] else tail(cand, 1)
  }, character(1))
  
  current <- current[!names(current) %in% feature_ids]
  features_list(current)
  
  updateFeaturesTable()
  updateFeatureLabels()
  showNotification(paste("Feição", paste(feature_ids, collapse = ", "), "foi deletada."), duration = 5, type = "message")
}