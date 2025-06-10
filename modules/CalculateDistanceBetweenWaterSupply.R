### 1. Function to calculate the distance ----
calculate_distance <- function(sistema_name, drawn_point, dataset) {
  # Filter the dataset for the selected "sistema" name
  selected_points <- dataset %>% filter(nm_captaca == sistema_name)
  selected_points <- st_cast(selected_points, "POINT")
  
  # Calculate the centroid of the selected points
  centroid <- st_centroid(st_union(selected_points))
  
  # Convert the drawn point to sf object
  drawn_sf <- st_sfc(st_point(drawn_point), crs = st_crs(dataset))
  
  # Calculate the distance
  distance <- as.numeric(st_distance(drawn_sf, centroid))
  print(distance)
  return(list(distance = distance, centroid = centroid, drawn_point = drawn_sf, 
              selected_points = selected_points))
}