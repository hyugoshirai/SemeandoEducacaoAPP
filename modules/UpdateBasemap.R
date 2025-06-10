### 1. Update the basemap when selection changes
### This module allows the user to select a different basemap for the leaflet map.
### It uses the leafletProxy function to update the map without reloading it.
### The module consists of two parts:

### 1. UI: A dropdown menu for selecting the basemap.
UpdateBasemapUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("basemap"),
      "Choose Basemap:",
      choices = list("OpenStreetMap" = "OpenStreetMap", "Satellite" = "Satellite")
    )
  )
}

### 2. Server: An observer that listens for changes in the dropdown and updates the map accordingly.
UpdateBasemapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$basemap, {
        leafletProxy("map") %>%
          clearTiles() %>%
          addProviderTiles(providers[[input$basemap]], group = "OpenStreetMap", layerId = "basemap")
      })  
    }
  )
}