library(googledrive)
library(httr)
library(sf)  # Load the 'sf' package for handling spatial objects

# Function to read an RDS file from Google Drive and process as an sf object
ReadGdriveRDS <- function(file_id) {
  # Construct the download URL
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
  
  # Make a GET request to download the RDS file
  response <- GET(url)
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Create a raw connection from the response content
    cn <- rawConnection(httr::content(response, "raw"))
    
    # Read the RDS file from the raw connection
    df <- readRDS(gzcon(cn))
    
    # Close the raw connection
    close(cn)
    
    # Check if the object is an 'sf' object (optional but recommended)
    if (!inherits(df, "sf")) {
      stop("The loaded object is not an 'sf' object.")
    }
    
    # Return the RDS object
    return(df)
  } else {
    stop("Failed to download the file. HTTP status code: ", status_code(response))
  }
}

# Example usage
# file_id <- "1gDp2yHcvkd8oaE9U-QnYDSeMfni5KjOT"
# df <- ReadGdriveRDS (file_id)
# 
# # If the object is an 'sf' object, plot it
# if (inherits(df, "sf")) {
#   plot(df)
# }