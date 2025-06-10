# This function updates the draw toolbar in a Leaflet map.
updateDrawToolbar <- function(session, color, fill_color) {
  
  # Define custom icon for the markers
  custom_icon <-   makeAwesomeIcon(
    icon = "home",
    library = "glyphicon",
    markerColor = color,
    iconColor = color,
    spin = FALSE,
    extraClasses = NULL,
    squareMarker = FALSE,
    iconRotate = 0,
    fontFamily = "monospace",
    text = NULL
  )
  
  leafletProxy("map", session = session) %>%
    removeDrawToolbar() %>%
    addDrawToolbar(
      polylineOptions = drawPolylineOptions(
        shapeOptions = drawShapeOptions(color = color)
      ),
      polygonOptions = drawPolygonOptions(
        showArea = TRUE, metric = TRUE,
        shapeOptions = drawShapeOptions(color = color, fillColor = fill_color), 
        repeatMode = FALSE
      ),
      circleOptions = FALSE,
      rectangleOptions = drawRectangleOptions(
        showArea = TRUE, metric = TRUE,
        shapeOptions = drawShapeOptions(color = color, fillColor = fill_color), 
        repeatMode = FALSE
      ),
      markerOptions = drawMarkerOptions(markerIcon = custom_icon),
      circleMarkerOptions = FALSE, 
      singleFeature = FALSE,
      editOptions = editToolbarOptions()
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