# Define server logic
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  ### 1. Initialize leaflet map ----
  output$map <- renderLeaflet({
    initializeMap(ProjectArea = `Sistema Cantareira`, legend_title = "Legenda dos pontos mapeados", label = label, legend_df = color_mapping, Phito = Fitofisionomias, StateLimits = `Limites estaduais`, ProtectedAreas = `Unidades de conservação`, LandUse_rst = `Uso do solo`, CityLimits = `Limites municipais`, UGRHI = `UGRHI`, Biomes = `Biomas`)
  })
  
  # ### 2. Update the basemap when selection changes ----
  # Custom basemap selector logic
  observeEvent(input$basemap, {
    active_overlays <- input$overlays  # Save which overlays are checked
    # Temporarily deselect all overlays
    updateCheckboxGroupInput(session, "overlays", selected = character(0))
    proxy <- leafletProxy("map")
    # Clear basemap tiles and add new basemap
    proxy %>% clearTiles()
    if (input$basemap == "OpenStreetMap") {
      proxy %>% addTiles(group = "OpenStreetMap")
    } else if (input$basemap == "Satellite") {
      proxy %>% addTiles(
        urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
        group = "Satellite"
      )
    }
    # Use a short delay to ensure the UI updates before re-selecting
    shinyjs::delay(50, {
      updateCheckboxGroupInput(session, "overlays", selected = active_overlays)
    })
  })
  
  # Overlay toggling logic (works for overlays only)
  observe({
    checked <- input$overlays
    proxy <- leafletProxy("map")
    for (g in names(legends_list)) {
      leg <- legends_list[[g]]
      if (!is.null(checked) && g %in% checked) {
        showGroup(proxy, g)
        if (!is.null(leg$pal) && !is.null(leg$values) && length(leg$values) > 0) {
          # Standard palette legend
          legend_args <- list(
            layerId = leg$layerId,
            position = "bottomright",
            pal = leg$pal,
            values = leg$values,
            title = leg$title,
            opacity = 1
          )
          if (!is.null(leg$labFormat)) legend_args$labFormat <- leg$labFormat
          do.call(addLegend, c(list(proxy), legend_args))
        } else if (!is.null(leg$pal) && is.null(leg$values)) {
          # Single color legend
          proxy %>% addLegend(
            layerId = leg$layerId,
            position = "bottomright",
            colors = leg$pal,
            labels = leg$title,
            opacity = 1
          )
        }
        # If neither, skip legend for this group
      } else {
        hideGroup(proxy, g)
        proxy %>% removeControl(layerId = leg$layerId)
      }
    }
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
    filename = function() paste0("Mapeamento_", Sys.Date(), ".zip"),
    content = function(file) {
      fl <- features_list()
      req(length(fl) > 0)
      
      parts <- lapply(fl, function(x) {
        # keep core columns
        geom_col <- attr(x, "sf_column")
        keep <- c("id", "name", "Tipo", "color", geom_col)
        x <- x[, keep[keep %in% names(x)], drop = FALSE]
        # types + CRS
        x$id <- as.character(x$id)
        if (!"name" %in% names(x)) x$name <- NA_character_
        if (!"Tipo" %in% names(x)) x$Tipo <- NA_character_
        if (!"color" %in% names(x)) x$color <- get_tipo_color(x$Tipo)
        x <- if (is.na(sf::st_crs(x))) { sf::st_set_crs(x, 4326) } else { sf::st_transform(x, 4326) }
        x
      })
      
      # Align columns just in case
      all_cols <- Reduce(union, lapply(parts, names))
      geom_col <- attr(parts[[1]], "sf_column")
      attr_order <- setdiff(all_cols, geom_col)
      parts <- lapply(parts, function(x) {
        miss <- setdiff(all_cols, names(x))
        for (m in miss) x[[m]] <- NA
        x[, c(attr_order, geom_col), drop = FALSE]
      })
      combined_sf <- do.call(rbind, parts)
      
      types <- sf::st_geometry_type(combined_sf)
      points_sf    <- combined_sf[grepl("POINT",   types), ]
      polygons_sf  <- combined_sf[grepl("POLYGON", types), ]
      polylines_sf <- combined_sf[grepl("LINE",    types), ]
      
      tmp <- tempdir()
      files <- character()
      if (nrow(points_sf) > 0) {
        nm <- "PontosMapeados"
        sf::st_write(points_sf, dsn = file.path(tmp, nm), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
        files <- c(files, list.files(tmp, pattern = paste0(nm, ".*$"), full.names = TRUE))
      }
      if (nrow(polygons_sf) > 0) {
        nm <- "PoligonosMapeados"
        sf::st_write(polygons_sf, dsn = file.path(tmp, nm), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
        files <- c(files, list.files(tmp, pattern = paste0(nm, ".*$"), full.names = TRUE))
      }
      if (nrow(polylines_sf) > 0) {
        nm <- "LinhasMapeadas"
        sf::st_write(polylines_sf, dsn = file.path(tmp, nm), driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
        files <- c(files, list.files(tmp, pattern = paste0(nm, ".*$"), full.names = TRUE))
      }
      zip::zipr(zipfile = file, files = files)
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
        paste("Distancia até o centro do sistema:", round(as.numeric(dist_sist_outp$distance), 2)/1000, "quilômetros")
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
  
  # Modal para carregar .zip do ambiente
  observeEvent(input$load_env_btn, {
    showModal(modalDialog(
      title = "Carregar ambiente",
      fileInput("env_zip", "Selecione o arquivo .zip do ambiente", accept = ".zip"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Carregar .zip, atualizar mapa e sincronizar lista de feições
  observeEvent(input$env_zip, {
    req(input$env_zip)
    message("==> Loading environment ZIP...")
    
    tmp <- tempfile(); dir.create(tmp)
    unzip(input$env_zip$datapath, exdir = tmp)
    shp_files <- list.files(tmp, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
    message("Shapefiles found: ", paste(basename(shp_files), collapse = ", "))
    req(length(shp_files) > 0)
    
    sfs <- lapply(shp_files, function(f) { message("Reading: ", f); sf::st_read(f, quiet = TRUE) })
    message("Rows per file: ", paste(sapply(sfs, nrow), collapse = ", "))
    combined_sf <- do.call(rbind, sfs)
    message("Total features loaded: ", nrow(combined_sf))
    
    # CRS unify
    if (is.na(sf::st_crs(combined_sf))) {
      sf::st_crs(combined_sf) <- 4326
      message("CRS missing; set to EPSG:4326")
    } else {
      combined_sf <- sf::st_transform(combined_sf, 4326)
      message("CRS transformed to EPSG:4326")
    }
    
    # Single color column
    if (!"color" %in% names(combined_sf) && "._color" %in% names(combined_sf)) {
      combined_sf$color <- combined_sf$._color
    }
    if (!"color" %in% names(combined_sf)) {
      if (!"Tipo" %in% names(combined_sf) && "label" %in% names(combined_sf)) {
        combined_sf$Tipo <- as.character(combined_sf$label)
      }
      combined_sf$color <- get_tipo_color(as.character(combined_sf$Tipo))
    }
    combined_sf$._color <- NULL
    
    # Core columns
    if (!"id" %in% names(combined_sf)) combined_sf$id <- paste0("loaded_", seq_len(nrow(combined_sf)))
    if (!"name" %in% names(combined_sf)) combined_sf$name <- NA_character_
    if (!"Tipo" %in% names(combined_sf) && "label" %in% names(combined_sf)) combined_sf$Tipo <- as.character(combined_sf$label)
    combined_sf$id    <- as.character(combined_sf$id)
    combined_sf$name  <- as.character(combined_sf$name)
    combined_sf$Tipo  <- as.character(combined_sf$Tipo)
    combined_sf$color <- as.character(combined_sf$color)
    
    # Log each feature
    gtypes <- as.character(sf::st_geometry_type(combined_sf))
    for (i in seq_len(nrow(combined_sf))) {
      message(sprintf(
        "[%d/%d] id=%s, name=%s, tipo=%s, geom=%s, color=%s",
        i, nrow(combined_sf),
        combined_sf$id[i],
        combined_sf$name[i],
        combined_sf$Tipo[i],
        gtypes[i],
        combined_sf$color[i]
      ))
    }
    
    # Append to map (do not clear previous)
    types <- sf::st_geometry_type(combined_sf)
    pts_idx   <- grepl("POINT",   types)
    lns_idx   <- grepl("LINE",    types)
    polys_idx <- grepl("POLYGON", types)
    
    proxy <- leafletProxy("map")
    if (any(pts_idx)) {
      proxy %>% addCircleMarkers(
        data = combined_sf[pts_idx, ],
        layerId = combined_sf$id[pts_idx],
        radius = 6,
        color = ~color, fillColor = ~color,
        fillOpacity = 0.7, stroke = TRUE, group = "Ambiente carregado"
      )
      message("Added points: ", sum(pts_idx))
    }
    if (any(lns_idx)) {
      proxy %>% addPolylines(
        data = combined_sf[lns_idx, ],
        layerId = combined_sf$id[lns_idx],
        color = ~color, weight = 3, group = "Ambiente carregado"
      )
      message("Added lines: ", sum(lns_idx))
    }
    if (any(polys_idx)) {
      proxy %>% addPolygons(
        data = combined_sf[polys_idx, ],
        layerId = combined_sf$id[polys_idx],
        color = ~color, fillColor = ~color,
        fillOpacity = 0.4, weight = 1, group = "Ambiente carregado"
      )
      message("Added polygons: ", sum(polys_idx))
    }
    
    # Append to features_list, keeping existing ones and resolving duplicate ids
    current <- features_list()
    existing_ids <- names(current)
    geom_col <- attr(combined_sf, "sf_column")
    core_cols <- c("id", "name", "Tipo", "color", geom_col)
    combined_core <- combined_sf[, core_cols[core_cols %in% names(combined_sf)], drop = FALSE]
    
    new_ids <- as.character(combined_core$id)
    if (length(existing_ids)) {
      uniq <- make.unique(c(existing_ids, new_ids), sep = "_dup")
      new_ids_unique <- tail(uniq, length(new_ids))
      if (!all(new_ids_unique == new_ids)) {
        message("Duplicate ids detected. Renaming loaded ids:")
        for (i in seq_along(new_ids)) {
          if (new_ids_unique[i] != new_ids[i]) {
            message("  ", new_ids[i], " -> ", new_ids_unique[i])
          }
        }
        combined_core$id <- new_ids_unique
      }
    }
    
    new_list <- lapply(seq_len(nrow(combined_core)), function(i) combined_core[i, ])
    names(new_list) <- combined_core$id
    features_list(c(current, new_list))
    
    message("Appended features. Previously: ", length(current),
            " | Loaded now: ", length(new_list),
            " | Total: ", length(features_list()))
    message("All feature ids: ", paste(names(features_list()), collapse = ", "))
    
    # Refresh DT via proxy (append effect)
    tbl <- tryCatch(features_table_data(), error = function(e) {
      # Minimal fallback
      fl <- features_list()
      if (length(fl) == 0) return(data.frame())
      do.call(rbind, lapply(fl, function(x) {
        data.frame(
          id   = as.character(x$id[1]),
          name = if ("name" %in% names(x)) as.character(x$name[1]) else NA_character_,
          Tipo = if ("Tipo" %in% names(x)) as.character(x$Tipo[1]) else NA_character_,
          color= if ("color" %in% names(x)) as.character(x$color[1]) else NA_character_,
          geom = as.character(sf::st_geometry_type(x))[1],
          stringsAsFactors = FALSE
        )
      }))
    })
    
    output$features_table <- rendered_table
    updateFeaturesTable() # Update the data table
    updateFeatureLabels() # Update feature labels
    
    removeModal()
    showNotification("Ambiente carregado e novos itens anexados.", type = "message")
  })
}