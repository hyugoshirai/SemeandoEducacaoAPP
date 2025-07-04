# Function to read a GeoJSON file from Google Drive in R

# Load necessary library
if (!require("sf")) install.packages("sf")
library(sf)

# Function to read remote GeoJSON files from Google Drive
ReadGDriveGeoJson <- function(file_id) {
  
  # Create the direct download URL
  download_link <- paste0("https://drive.usercontent.google.com/download?id=", file_id)

  # Read the GeoJSON file directly from the URL using st_read
  geojson_data <- st_read(download_link)
  
  # Print and plot to check the data
  print(geojson_data)
  return(geojson_data)
}

# # # # Example usage
# # # # Input shareable link to a GeoJson file on Google Drive
# geojson_data <- ReadGDriveGeoJson("1I1omBZzCcNqlEUSzWSmjZAArl_x0ZehZ")
# # # # Optionally plot the data
# plot(geojson_data)
