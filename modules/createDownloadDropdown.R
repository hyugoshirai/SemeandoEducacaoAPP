### 1. Function to get the names of the current raster layers ----
###   This function retrieves the names of the current raster layers and shapefiles
getCurrentLayerNames <- reactive({
  raster_names <- names(all_reclassified_rasters())
  # shapefile_list <- shapefile_names_list()
  shapefiles_names <- names (all_shapefiles())
  # Combine raster names with shapefile names
  c(names(Uso_do_Solo), raster_names, shapefiles_names)#, "Result raster", shapefile_list)
})



### 2. Function to create the UI for all dropdowns ----
###   This function creates a dropdown menu for selecting layers
createDownloadDropdown <- function(id, multiple = TRUE, populated = FALSE, current_layers, label =  "Select Layers:") {
  # Get the current layer names
  # Create a fluidRow with one column containing the dropdown
  fluidRow(
    column(6,
           # Dropdown for raster selection with multiple selection enabled
           selectInput(id,
                       label = label,
                       choices = c("Select Layers" = "", current_layers),
                       multiple = multiple  # Allow multiple selection
           )
    )
  )
}