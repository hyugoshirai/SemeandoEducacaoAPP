### 1.  Function to add the shapefile to the map
### This function adds a shapefile to the map and updates the layers control
### sistema_outp: Reactive list containing the drawn points, centroid, and selected points

addCaptacaoToMap <- function(sistema_outp, sistema_name) {
  coords_drawn <- sistema_outp$drawn_point$geometry$coordinates
  addShp <- leafletProxy("map") %>%
    clearGroup("escola") %>%
    clearGroup(sistema_name) %>%
    clearGroup("linha") %>%
    clearGroup("pontos") %>%
    addCircleMarkers(data = sistema_outp$drawn_point, 
                     group = "escola", 
                     popup = "ESCOLA", color = "red") %>%
    addCircleMarkers(data = sistema_outp$centroid, 
                     color = "blue",
                     popup = paste (
                       "Nome do sistema: ", sistema_name),
                     group = sistema_name) %>%
    addCircleMarkers(data = sistema_outp$selected_points,
                     color = "green",
                     popup = paste (
                       "Nome do sistema: ", sistema_outp$selected_points$nm_captaca, "<br>",
                       "Tipo de capitação: ", sistema_outp$selected_points$tp_captaca,"<br>",
                       "Nome do ponto :", sistema_outp$selected_points$nm_fantasi),
                     group = "pontos")|> 
    addPolylines(lng = c(st_coordinates(sistema_outp$drawn_point)[1], st_coordinates(sistema_outp$centroid)[1]),
                 lat = c(st_coordinates(sistema_outp$drawn_point)[2], st_coordinates(sistema_outp$centroid)[2]),
                 color = "purple",
                 group = "linha") |> 
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"), # Always include base groups
      overlayGroups = c(custom_control),  # Ensure all layers, including new ones, are listed
      options = layersControlOptions(collapsed = TRUE)
    )
}
