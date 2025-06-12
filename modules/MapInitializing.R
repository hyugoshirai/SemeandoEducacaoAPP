### 1. Function to initialize the map with the land use raster image
### This function creates a leaflet map with the land use raster image and other layers

initializeMap <- function(ProjectArea, Phito, StateLimits, CityLimits, ProtectedAreas, LandUse_rst, UGRHI, Biomes,  legend_title = "Legenda", label, legend_df) {
  
  # # aggregate the raster for faster visualization
  factor <- 5
  Uso_do_Solo_agg <- raster::aggregate(LandUse_rst, fact = factor, fun = modal)
  
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}", group = "Satellite") %>%
    #==================== ProjectArea ====================
  # Add Área do Projeto layer
  addPolygons(data = ProjectArea,
              color = "red",    # Border color of polygons
              weight = 1.5,        # Border width of polygons
              opacity = 1,       # Border opacity
              fillOpacity = 0,
              group = "Sistema Cantareira"
  ) |>
    #==================== StateLimits ====================
  # Add State limits layer
  addPolygons(data = StateLimits,
              color = "black",    # Border color of polygons
              weight = 1,        # Border width of polygons
              opacity = 1,       # Border opacity
              group = "Limites estaduais",
              fillColor = "grey",
              fillOpacity = 0.8,
              popup = ~paste("Nome da UF: ", NM_UF)
  ) |>
    #==================== CityLimits ====================
  # Add Área do Projeto layer
  addPolygons(data = CityLimits,
              color = "blue",    # Border color of polygons
              weight = 1,        # Border width of polygons
              opacity = 1,       # Border opacity
              group = "Limites municipais",
              fillColor = "grey",
              fillOpacity = 0.8,
              popup = ~paste("Nome da Município: ", NM_MUN)
  ) |>
    #==================== UGRHI ====================
  # Add Área do Projeto layer
  addPolygons(data = UGRHI,
              color = "grey",    # Border color of polygons
              weight = 1,        # Border width of polygons
              opacity = 1,       # Border opacity
              group = "UGRHI",
              fillColor = "lightblue",
              fillOpacity = 0.8,
              popup = ~paste("Código da UGRHI: ", Codigo, "<br>",
                             "Nome da UGRHI: ", Nome)
  ) |>
    #==================== Phito ====================
  #  #Add Phito layer
  addPolygons(data = Phito,
              color = ~Phito_pal (Phito_labels),
              weight = 1,
              opacity = 1,
              fillOpacity = 0.5,
              group = "Fitofisionomias",
              # show in the popup thhe desc_subcl describing subclasses and area
              popup = ~legenda) |>
    # Phito legend
    addLegend(
      position = "bottomleft",
      colors = Phito_colors,
      labels = Phito_labels,
      title = "Fitofisionomias",
      group = "Fitofisionomias"
    ) |>
    #==================== Uso_do_Solo ====================
  addRasterImage(Uso_do_Solo_agg,
                 colors = land_use_pal,
                 group = "Uso do solo",
                 project = TRUE,
                 method = 'ngb'
  ) |> 
    #==================== Biomes ====================
  #  #Add Biomes layer
  addPolygons(data = Biomes,
              color = ~Biomes_pal (Biomes_labels),
              weight = 1,
              opacity = 1,
              fillOpacity = 0.5,
              group = "Biomas",
              # show in the popup thhe desc_subcl describing subclasses and area
              popup = ~Bioma) |>
    # Biomes legend
    addLegend(
      position = "bottomleft",
      colors = Biomes_colors,
      labels = Biomes_labels,
      title = "Biomas",
      group = "Biomas"
    ) |>
    # Land use legend
    addLegend(
      position = "bottomleft",
      pal = land_use_pal,
      values = sort(unique(values(LandUse_rst))),
      title = "Uso do solo",
      labFormat = labelFormat(transform = function(x) landuse_df$land_use[match(x, landuse_df$raster_value)]),
      opacity = 1,
      group = "Uso do solo"  # Group the legend with the raster layer
    )%>% 
    #   #==================== Áreas Protegidas ====================
  #Add Areas Protegidas layer
  addPolygons(
    data = ProtectedAreas,
    color = ~spa_pal(Tipo),
    weight = 1,
    opacity = 1,
    fillOpacity = 0.5,
    group = "Unidades de conservação",
    popup = ~paste("Nome da UC: ", NOME_UC1, "Categoria: ", CATEGORI3, 
                   "Codigo: ", CODIGO_U11)
    
  ) %>%
    # Add a legend for the special areas
    addLegend(
      pal = spa_pal,
      values = spa_labels,
      title = "Unidades de conservação",
      position = "bottomleft",
      group = "Unidades de conservação"  # Group the legend with the raster layer
    ) |>
    # Draw tools
    addDrawToolbar(
      polylineOptions = TRUE,
      polygonOptions = drawPolygonOptions(),
      # circleOptions = drawCircleOptions(),
      circleOptions = FALSE,
      rectangleOptions = drawRectangleOptions(),
      markerOptions = drawMarkerOptions(), # Enable marker drawing
      circleMarkerOptions = FALSE, 
      singleFeature = FALSE,
      editOptions = editToolbarOptions()
    ) %>%
    addScaleBar(
      position = "bottomright",
      options = scaleBarOptions(imperial = FALSE, metric = TRUE)
    ) %>%
    addSearchOSM(
      options = searchOptions(autoCollapse = TRUE, minLength = 2)
    ) %>%
    addMeasure(
      position = "topright",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters"
    ) %>%
    # Layer control
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = c(custom_control),  # Ensure unique layers are listed
      options = layersControlOptions(collapsed = TRUE)
    ) |> 
    hideGroup(c("Limites estaduais", "Unidades de conservação", "Uso do solo", "Fitofisionomias", "UGRHI", "Biomas", "Limites municipais")) |> 
    fitBounds(lng1 = min(st_bbox(ProjectArea)[c("xmin")]), 
              lat1 = min(st_bbox(ProjectArea)[c("ymin")]), 
              lng2 = max(st_bbox(ProjectArea)[c("xmax")]), 
              lat2 = max(st_bbox(ProjectArea)[c("ymax")])
    )%>%
    addLegend(
      position = "bottomleft",
      title = legend_title,
      colors = unique(legend_df$color),
      labels = unique(legend_df$label),
      opacity = 1
    )
}
