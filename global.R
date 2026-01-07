# Load required packages ----
# library("classInt")
library("dplyr")
library ("DT")
library("gdistance")
# library("leafem")
library("leafem")
library("leaflet")
library("leaflet.extras")
library("leastcostpath")
# library("mapview")
library("raster")
# library("RColorBrewer")
library("shiny")
# library("shinyFiles")
# library("shinyWidgets")
# library("shinyalert")
library("shinyjs")
library("sf")
library("terra")
# library("tools")
# library("units")
library("zip")

#### Source Modules
# Define the directory containing the R script files
modules_directory <- "modules"

# List all .R files in the directory
script_files <- list.files(modules_directory, pattern = "\\.R$", full.names = TRUE)
# Modules to be sourced after initializing objects
late_objects_modules <- c("DefineLabelsAndColors.R")
script_files <- script_files[!basename(script_files) %in% late_objects_modules] # Exclude late objects modules

# Loop through each R file and source it
for (script_file in script_files) {
  source(script_file)
  print (paste("Sourced", script_file))
}

### Initialize objects ----

# Global variable to store names of assigned files
default_layers_names <- c()

# Create objects from sheet 
AssignObjectsFromGsheet("https://docs.google.com/spreadsheets/d/1eRSEmnMIIEcS6EUkhOguy_mCRpZRN1vy/edit?usp=sharing&ouid=102538809962333046552&rtpof=true&sd=true")

# Initialize the lists to store the default objects
default_shapefiles <- list() # List for vectors
default_layers <- list() # List for rasters

# Add the objects to the reactive lists based on their type
CategorizeAndReprojectDefaultObjects()

# Now source late modules that depend on the initialized objects
for (late_module in late_objects_modules) {
  source(file.path(modules_directory, late_module))
  print(paste("Sourced late module:", late_module))
}

# Define custom control names
custom_control <- setdiff (c(names (default_layers), names (default_shapefiles)),
                           c("Estações de tratamento", "Pontos de captação"))

### 5. Define reactive values  ----
all_points <- reactiveVal(list()) # For storing all points
all_reclassified_rasters <- reactiveVal(list()) # Create a reactive list to hold all reclassified rasters
all_shapefiles <- reactiveVal(list()) # For storing the shapefiles
checkbox_layers <- reactiveVal(c("Original Raster")) # Initialize a reactive value to store the checkbox layers
current_layers <- reactiveVal(character(0)) # Create a reactive list to hold current layers name
default_layers_name <- reactiveVal() # Create a reactive value to store the default layers name
default_layers_reclass_df <- reactiveVal() # Create a reactive value to store the default layers reclassification dataframe
features_list <- reactiveVal(list())  # Create a reactive value to store the features list
dinamic_added <- reactiveVal(list()) # For storing dynamic added layers
downldropdown <- reactiveValues(dropdowns = "layers_dropdown") # Reactive value to store the dropdown identifier
euclidean_dist_rasters <- reactiveValues() # Create a reactive value to store the euclidean distance rasters
euclidean_reclass_df <- reactiveVal() # Reactive value to store the reclassification dataframe for euclidean distance
feature_values_data <- reactiveVal(NULL) # For storing feature values data
layer_colors <- reactiveValues(list = list()) # Redirect layer colors to observe later listItem
rasterized_layer <- reactiveVal(NULL) # For storing rasterized shapefile
reactive_tabs <- reactiveVal(list()) # For storing tab names
raster_data_processed <- reactiveVal(NULL) # For storing the processed raster data
# Rec_LandUse <- reactiveVal(LandUse_rst) # For storing reclassified LandUse data
raster_df <- reactiveVal() # For storing raster data frames
raster_df_list <- reactiveVal(list()) # For storing raster data frames list
rendered_table <- renderDT({
  features_table_data()
}, editable = list(target = "cell", disable = list(columns = c(0, 1, 3, 4))), selection = 'multiple', server = FALSE) 
# Render the features table
result_raster <- reactiveVal(NULL) # For storing result raster
shapefile_data <- reactiveValues(shp = NULL, colors = list()) # For storing the uploaded shapefile data
shapefile_data_processed <- reactiveVal(NULL) # For storing the processed shapefile data
shapefile_names_list <- reactiveVal(character(0)) # For storing the names of uploaded shapefiles
shortest_path_counter <- reactiveVal(0) # Counter for shortest path
shortest_path_layers <- reactiveValues(list = list()) # Create a reactive value to store the shortest path layers
shortest_path_result <- reactiveVal(NULL) # For storing shortest path result
selected_layer <- reactiveVal(NULL) # For storing the selected layer in the dropdown list