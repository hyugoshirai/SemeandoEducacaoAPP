### 1. Update feature labels on the map
### This function updates the labels of the features on the map.
updateFeatureLabels <- function() {
  proxy <- leafletProxy("map") %>% clearGroup("feature_labels")
  fl <- features_list()
  if (length(fl) == 0) return(invisible(NULL))
  
  for (fid in names(fl)) {
    feat <- fl[[fid]]
    
    # Skip if not an sf or no geometry
    if (!inherits(feat, "sf") || is.null(sf::st_geometry(feat)) || length(sf::st_geometry(feat)) == 0) {
      message("updateFeatureLabels: skipping id=", fid, " (no geometry)")
      next
    }
    
    # Safe anchor point for any geometry
    anchor <- tryCatch(sf::st_point_on_surface(feat), error = function(e) NULL)
    if (is.null(anchor)) {
      message("updateFeatureLabels: could not compute anchor for id=", fid)
      next
    }
    coords <- sf::st_coordinates(anchor)
    lng <- coords[1]; lat <- coords[2]
    
    lbl <- if ("name" %in% names(feat)) as.character(feat$name[1]) else fid
    
    proxy %>%
      addLabelOnlyMarkers(
        lng = lng, lat = lat,
        label = HTML(paste0('<div style="background-color: white; border: 2px solid black; padding: 1px; border-radius: 1px;">
                               <span style="color: black; font-size: 14px; font-weight: bold;">', lbl, '</span>
                             </div>')),
        labelOptions = labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE),
        group = "feature_labels"
      )
  }
}