# Function to read the Google Sheet and assign objects
AssignObjectsFromGsheet <- function(sheet_url, filter_features) {
  
  # Construct the CSV URL from the sheet URL
  sheet_id <- sub(".*\\/d\\/([a-zA-Z0-9_-]+).*", "\\1", sheet_url)
  csv_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv")
  
  # Read the CSV file into a data frame
  df <- read.csv(csv_url)
  # Filter by name on list
  if (!missing(filter_features)) {
    df <- df[df$Nome %in% filter_features, ]
  }
  # print (df)
  # Loop through each row of the dataframe
  for (i in 1:nrow(df)) {
    file_name <- df$Nome[i]
    file_type <- df$Tipo[i]
    file_id <- df$id[i]
    
    if (file_type == "Raster") {
      assign(file_name, ReadGDriveRaster(file_id), envir = .GlobalEnv)
    # } else if (file_type == "GeoJSON") {
    #   assign(file_name, ReadGDriveGeoJson(file_id), envir = .GlobalEnv)
    } else if (file_type == "RDS") {
      assign(file_name, ReadGdriveRDS(file_id), envir = .GlobalEnv)
    }
    
    # Add the file name to the global list
    default_layers_names <<- c(default_layers_names, file_name)
  }
}

# # # Example usage:
# Input Google Sheet shareable URL
# AssignObjectsFromGsheet("https://docs.google.com/spreadsheets/d/1AR3T45pZ2y5CO1A2PYKXn4HxVh9xyB7CZnjnY1zy26E/edit?usp=sharing")
# 
# # Check if the objects are assigned (example)
# plot (`Imóveis`)
# plot (`Uso do solo`)