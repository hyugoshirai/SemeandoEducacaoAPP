# Define server logic
server <- function(input, output, session) {
  
  ### 1. Initialize leaflet map ----
  output$map <- renderLeaflet({
    initializeMap(ProjectArea = `Sistema Cantareira`, legend_title = "Legend", label = label, legend_df = color_mapping, Phito = Fitofisionomias, StateLimits = `Limites estaduais`, ProtectedAreas = `Unidades de conservação`, LandUse_rst = `Uso do solo`, CityLimits = `Limites municipais`, UGRHI = `UGRHI`, Biomes = `Biomas`)
  })
  
  ### 2. Update the basemap when selection changes ----
  observeEvent(input$basemap, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(providers[[input$basemap]])
  })
  
  ### Observe drawing features on the map ----
  # Observe inputs on mapping dropdown and update the draw toolbar colors
  observe({
    label <- input$MappingInput
    color_info <- color_mapping %>% filter(label == !!label)
    color <- color_info$color
    fill_color <- color
    
    updateDrawToolbar(session, color, fill_color)
  })
  
  # Update the features list when a new feature is drawn 
  observeEvent(input$map_draw_new_feature, {
    handleNewFeature(input, input$map_draw_new_feature)
    # Render features table
    output$features_table <- rendered_table
  })
  
  # Update the features list when a feature is edited
  observeEvent(input$map_draw_edited_features, {
    handleFeatureEdit(input, input$map_draw_edited_features)
    # Render features table
    output$features_table <- rendered_table
  })
  
  # Observer for cell edits in the data table
  observeEvent(input$features_table_cell_edit, {
    handleCellEdit(input, input$features_table_cell_edit)
    # Render features table
    output$features_table <- rendered_table
  })
  
  # Handle feature deletion
  observeEvent(input$map_draw_deleted_features, {
    handleFeatureDeletion(input, input$map_draw_deleted_features)
    # Render features table
    output$features_table <- rendered_table
  })
  
  ### Download handler for shapefile download ----
  output$download_shapefile <- downloadHandler(
    filename = function() {
      paste("drawn_features", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Combine all features into one sf object
      combined_sf <- combineFeatures()
      # Check CRS
      crs_info <- st_crs(combined_sf)
      # If CRS is missing, set it to the known CRS
      if (is.na(st_crs(combined_sf))) {
        st_crs(combined_sf) <- 4326  # Set CRS to WGS 84 if missing
      } else {
        # Reproject the features to WGS 84 if CRS is already defined
        combined_sf <- st_transform(combined_sf, crs = 4326)
      }
      
      # Separate points and polygons
      points_sf <- combined_sf[st_geometry_type(combined_sf) == "POINT", ]
      polygons_sf <- combined_sf[st_geometry_type(combined_sf) == "POLYGON", ]
      polylines_sf <- combined_sf[st_geometry_type(combined_sf) == "LINESTRING", ]
      
      
      # Create a temporary directory to save the shapefile components
      temp_shapefile_dir <- tempdir()
      
      shapefile_components <- c()
      
      # Write the points shapefile if it exists
      if (nrow(points_sf) > 0) {
        points_shapefile_name <- "drawn_points"
        st_write(points_sf, dsn = file.path(temp_shapefile_dir, points_shapefile_name), driver = "ESRI Shapefile", delete_dsn = TRUE)
        shapefile_components <- c(shapefile_components, list.files(temp_shapefile_dir, pattern = paste0(points_shapefile_name, ".*$"), full.names = TRUE))
      }
      
      # Write the polygons shapefile if it exists
      if (nrow(polygons_sf) > 0) {
        polygons_shapefile_name <- "drawn_polygons"
        st_write(polygons_sf, dsn = file.path(temp_shapefile_dir, polygons_shapefile_name), driver = "ESRI Shapefile", delete_dsn = TRUE)
        shapefile_components <- c(shapefile_components, list.files(temp_shapefile_dir, pattern = paste0(polygons_shapefile_name, ".*$"), full.names = TRUE))
      }
      
      # Write the polylines shapefile if it exists
      if (nrow(polylines_sf) > 0) {
        polylines_shapefile_name <- "drawn_polylines"
        st_write(polylines_sf, dsn = file.path(temp_shapefile_dir, polylines_shapefile_name), driver = "ESRI Shapefile", delete_dsn = TRUE)
        shapefile_components <- c(shapefile_components, list.files(temp_shapefile_dir, pattern = paste0(polylines_shapefile_name, ".*$"), full.names = TRUE))
      }
      
      # Create a zip file with the shapefile components
      zip::zipr(zipfile = file, files = shapefile_components)
    }
  )
  
  # Observing Help button click to trigger modal dialog
  observeEvent(input$help_button, {
    showModal(createHelpModal())
  })
  
  # Distance between point and the water supply system ----
  # Populate the dropdown with sorted and unique Sistema names 
  observe({
    sistema_names <- sort(unique(`Pontos de captação`$nm_captaca))
    updateSelectInput(session, "sistema", choices = sistema_names)
  })
  
  # Print name of sistema selected
  observe({
    selected_sistema <- input$sistema
    if (!is.null(selected_sistema)) {
      print(paste("Selected sistema:", selected_sistema))
      print (`Pontos de captação` %>% filter(nm_captaca == selected_sistema)) 
    }
  })
  
  observeEvent(input$calc_dist, {
    tryCatch({
      
      features_list <- features_list()
      print(features_list)
      filtered_features <- FilterFeaturebyTipo (features_list, "Escola")
      
      # Print the filtered list
      print(filtered_features)
      
      # Calculate the centroid of "Escola" features
      centroid <- st_centroid(st_union(do.call(rbind, filtered_features)))
      coords <- st_coordinates(centroid)
      
      # Calculate the distance using the function
      dist_sist_outp <- calculate_distance(input$sistema, c(coords[1], coords[2]), `Pontos de captação`)
      print(dist_sist_outp)
      
      # Output the distance
      output$distance_output <- renderText({
        paste("Distancia até o centro do sistema:", round(as.numeric(dist_sist_outp$distance), 2), "quilômetros")
      })
      
      #   # Update added point to list
      # updateList(sistema_outp$drawn_point, "escola", list = dinamic_added)
      # updateList(sistema_outp$centroid, input$sistema, list = dinamic_added)
      
      #   # Update added points to map
      addCaptacaoToMap(dist_sist_outp, input$sistema)
      
    }, error = function(e) {
      showNotification("Por favor, localize sua escola no mapa.", type = "error")
    })
  }) 
}