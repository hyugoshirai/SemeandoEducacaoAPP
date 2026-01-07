# This function updates the draw toolbar in a Leaflet map.
updateDrawToolbar <- function(session, color, fill_color, group = "Ambiente carregado") {
  custom_icon <- makeAwesomeIcon(
    icon = "home",
    library = "glyphicon",
    markerColor = color,   # must be one of the AwesomeMarkers palette names
    iconColor = color
  )
  
  leafletProxy("map", session = session) %>%
    removeDrawToolbar() %>%
    addDrawToolbar(
      targetGroup = group,  # this is the editable group
      polylineOptions = drawPolylineOptions(
        shapeOptions = drawShapeOptions(color = color)
      ),
      polygonOptions = drawPolygonOptions(
        showArea = TRUE, metric = TRUE,
        shapeOptions = drawShapeOptions(color = color, fillColor = fill_color, fillOpacity = 0.4),
        repeatMode = FALSE
      ),
      rectangleOptions = drawRectangleOptions(
        showArea = TRUE, metric = TRUE,
        shapeOptions = drawShapeOptions(color = color, fillColor = fill_color, fillOpacity = 0.4),
        repeatMode = FALSE
      ),
      circleOptions = FALSE,
      circleMarkerOptions = FALSE,
      markerOptions = drawMarkerOptions(markerIcon = custom_icon),
      singleFeature = FALSE,
      editOptions = editToolbarOptions(
        edit = TRUE,
        remove = TRUE
      )
    )
}

# # Example usage:
# updateDrawToolbar(session, color = "blue", fill_color = "lightblue")
# # Or observe input changes to dynamically update the toolbar
# observe({
#   color <- input$MappingInput
#   fill_color <- input$MappingInput
#   updateDrawToolbar(session, color, fill_color)
# })
# }